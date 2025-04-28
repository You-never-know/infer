(* Author: Dominik Harmim <iharmim@fit.vut.cz> *)

open! IStd
open AtomicityUtils
module F = Format

(** Detection of atomic sets domain implementation. *)

(* ************************************ Types and Modules *************************************** *)

(** A function call (with its location). *)
type call = string * (Location.t[@printer Location.pp_file_pos]) option
[@@deriving compare, equal, show {with_path= false}]

(** A set of calls. *)
module CallSet = MakePPSet (struct
  type t = call [@@deriving compare, equal, show {with_path= false}]
end)

(** A set of sets of calls. *)
module CallsSet = MakePPSet (CallSet)

(** A set of atomic calls. *)
type atomic_calls = CallSet.t * Lock.t [@@deriving compare, equal, show {with_path= false}]

(** A set of sets of atomic calls. *)
module AtomicCallsSet = MakePPSet (struct
  type t = atomic_calls [@@deriving compare, equal, show {with_path= false}]
end)

(* Type to save info about memory access *)
module MemoryAccess = struct
  type access_kind = Read | Write
  [@@deriving compare, equal, show {with_path= false}]

  type t = {
    access_path: HilExp.AccessExpression.t;
    location: Location.t;
    access_kind: access_kind;
    under_lock: bool;
    is_struct_field: bool;
  }
  [@@deriving compare, equal, show {with_path= false}]
end

module MemoryAccessSet = PrettyPrintable.MakePPSet (MemoryAccess)

(* ************************************ Astate ************************************************** *)

(** A pretty-printable function for printing 'CallSet.t list' with elements' indices. *)
let pp_calls : F.formatter -> CallSet.t list -> unit =
  pp_icollection ~pp_item:(fun (fmt : F.formatter) ((i : int), (calls : CallSet.t)) : unit ->
      F.fprintf fmt "%i: %a" i CallSet.pp calls )


(** An element of an abstract state. *)
type t_element =
  { atomic_calls: AtomicCallsSet.t
  ; final_atomic_calls: CallsSet.t
  ; calls: (CallSet.t list[@printer pp_calls])
  ; guards: Guards.t
  ; memory_accesses: MemoryAccessSet.t
  }
[@@deriving compare, equal, show {with_path= false}]

(** A set of types 't_element' is an abstract state. *)
module TSet = MakePPSet (struct
  type t = t_element [@@deriving compare, equal, show {with_path= false}]
end)

type t = TSet.t [@@deriving compare, equal, show {with_path= false}]

type astate = t [@@deriving compare, equal]

let initial : t =
  assert_user
    (Config.atomic_sets_widen_limit > 0)
    "Input argument '--atomic-sets-widen-limit' should be greater than 0, %d given."
    Config.atomic_sets_widen_limit ;
  assert_user
    (Config.atomic_sets_locked_functions_limit > 0)
    "Input argument '--atomic-sets-locked-functions-limit' should be greater than 0, %d given."
    Config.atomic_sets_locked_functions_limit ;
  assert_user
    (Config.atomic_sets_functions_depth_limit >= 0)
    "Input argument '--atomic-sets-functions-depth-limit' should be greater than or equal to 0, %d \
     given."
    Config.atomic_sets_functions_depth_limit ;
  (* An initial abstract state is a set with a single empty element. *)
  TSet.singleton
    { atomic_calls= AtomicCallsSet.empty
    ; final_atomic_calls= CallsSet.empty
    ; calls= [CallSet.empty]
    ; guards= Guards.empty
    ; memory_accesses= MemoryAccessSet.empty
    }

(** Reduces a set of calls: removes call locations if the call is already present. *)
let reduce_call_set ~(contains_call : call -> bool) : CallSet.t -> CallSet.t =
  CallSet.map (fun ((f, loc) as call : call) : call ->
      (f, if contains_call call then None else loc) )


(** Reduces a set of sets of calls: removes call locations if the call is already present. *)
let reduce_calls_set ~(contains_call : call -> bool) : CallsSet.t -> CallsSet.t =
  if not Config.atomic_sets_ignore_single_atomic_calls then Fn.id
  else CallsSet.map (reduce_call_set ~contains_call)


(** Checks whether it is the same call but different location. *)
let is_same_call_diff_loc ((f, loc) : call) ((f', loc') : call) : bool =
  String.equal f f' && not (Option.equal Location.equal loc loc')


(** Reduces atomic sets in an abstract state: removes call locations if the call is already present. *)
let reduce_atomic_calls (astate : t) : t =
  if not Config.atomic_sets_ignore_single_atomic_calls then astate
  else
    let mapper ({atomic_calls; final_atomic_calls; calls} as astate_el : t_element) : t_element =
      let atomic_calls : AtomicCallsSet.t =
        let contains_call (call : call) : bool =
          TSet.exists
            (fun ({atomic_calls} : t_element) : bool ->
              AtomicCallsSet.exists
                (Fn.compose (CallSet.exists (is_same_call_diff_loc call)) fst)
                atomic_calls )
            astate
        in
        AtomicCallsSet.map
          (fun ((calls, lock) : atomic_calls) : atomic_calls ->
            (reduce_call_set ~contains_call calls, lock) )
          atomic_calls
      and final_atomic_calls : CallsSet.t =
        let contains_call (call : call) : bool =
          TSet.exists
            (fun ({final_atomic_calls} : t_element) : bool ->
              CallsSet.exists (CallSet.exists (is_same_call_diff_loc call)) final_atomic_calls )
            astate
        in
        reduce_calls_set ~contains_call final_atomic_calls
      and calls : CallSet.t list =
        let contains_call (call : call) : bool =
          TSet.exists
            (fun ({calls} : t_element) : bool ->
              List.exists calls ~f:(CallSet.exists (is_same_call_diff_loc call)) )
            astate
        in
        List.map calls ~f:(reduce_call_set ~contains_call)
      in
      {astate_el with atomic_calls; final_atomic_calls; calls}
    in
    TSet.map mapper astate


(** Modifies an element of an abstract state after addition of function calls. *)
let update_astate_el_after_calls ({atomic_calls} as astate_el : t_element) : t_element =
  let atomic_calls : AtomicCallsSet.t =
    AtomicCallsSet.filter
      (fun ((calls, _) : atomic_calls) : bool ->
        CallSet.cardinal calls <= Config.atomic_sets_locked_functions_limit )
      atomic_calls
  in
  {astate_el with atomic_calls}


let apply_call ~(f_name : string) (loc : Location.t) : t -> t =
  let mapper ({atomic_calls; calls} as astate_el : t_element) : t_element =
    let loc_opt : Location.t option =
      if Config.atomic_sets_ignore_single_atomic_calls then Some loc else None
    in
    let atomic_calls : AtomicCallsSet.t =
      AtomicCallsSet.map
        (fun ((calls, lock) : atomic_calls) : atomic_calls ->
          (CallSet.add (f_name, loc_opt) calls, lock))
        atomic_calls
    in
    let calls : CallSet.t list =
      List.mapi calls ~f:(fun i ->
        if Int.equal i 0 then CallSet.add (f_name, loc_opt) else Fn.id)
    in
    update_astate_el_after_calls {astate_el with atomic_calls; calls}
  in
  Fn.compose reduce_atomic_calls (TSet.map mapper)

let apply_locks (locks_paths : AccessPath.t list) : t -> t =
  let mapper ({guards; atomic_calls} as astate_el : t_element) : t_element =
    let locks_paths : AccessPath.t list = Guards.reveal_locks guards locks_paths in
    let atomic_calls : AtomicCallsSet.t =
      let fold (atomic_calls : AtomicCallsSet.t) (path : AccessPath.t) : AtomicCallsSet.t =
        let found = ref false in
        let mapper ((calls, lock) as atomic_calls : atomic_calls) : atomic_calls =
          if AccessPath.equal path (Lock.path lock) then (
            found := true;
            (calls, Lock.lock lock))
          else atomic_calls
        in
        let atomic_calls = AtomicCallsSet.map mapper atomic_calls in
        if !found then atomic_calls
        else (
          AtomicCallsSet.add (CallSet.empty, Lock.create path) atomic_calls
        )
      in
      List.fold locks_paths ~init:atomic_calls ~f:fold
    in
    {astate_el with atomic_calls}
  in
  TSet.map mapper

  let custom_compare lock_path provided_path =
      let lock_base = fst lock_path in
      let unlock_base = fst provided_path in
      let lock_accesses = snd lock_path in
      let unlock_accesses = snd provided_path in
      let (lock_var, lock_typ) = lock_base in
      let (unlock_var, unlock_typ) = unlock_base in
      let var_to_string v = Exp.to_string (Var.to_exp v) in
      let base_equal =
        String.equal (var_to_string lock_var) (var_to_string unlock_var)
        && Typ.equal lock_typ unlock_typ
      in
      let access_equal = [%compare.equal: AccessPath.access list] lock_accesses unlock_accesses in
      base_equal && access_equal

let apply_unlocks (locks_paths : AccessPath.t list) : t -> t =
  let mapper ({guards; atomic_calls; final_atomic_calls} as astate_el : t_element) : t_element =
    let final_atomic_calls : CallsSet.t ref = ref final_atomic_calls
    and locks_paths : AccessPath.t list = Guards.reveal_locks guards locks_paths in
    let atomic_calls : AtomicCallsSet.t =
      let mapper ((calls, lock) as atomic_calls : atomic_calls) : atomic_calls option =
        let lock_path = Lock.path lock in
        let matched = List.exists locks_paths ~f:(fun provided_path ->
            (* Check equality using detailed printing for debugging *)
            let equal_paths = custom_compare provided_path lock_path in
            equal_paths
          )
        in
        if matched then (
          let lock = Lock.unlock lock in
          if (not (Lock.is_locked lock)) && CallSet.cardinal calls > 0 then (
            final_atomic_calls := CallsSet.add calls !final_atomic_calls
          ) ;
          if Lock.is_locked lock then Some (calls, lock) else None
        ) else (
          Some atomic_calls
        )
      in
      AtomicCallsSet.filter_map mapper atomic_calls
    in
    {astate_el with atomic_calls; final_atomic_calls= !final_atomic_calls}
  in
  Fn.compose reduce_atomic_calls (TSet.map mapper)


let apply_guard_construct (guard_path : AccessPath.t) (locks_paths : AccessPath.t list)
    ~(acquire : bool) : t -> t =
  let add_guard : t -> t =
    TSet.map (fun ({guards} as astate_el : t_element) : t_element ->
        {astate_el with guards= Guards.add guard_path locks_paths guards} )
  in
  if acquire then Fn.compose (apply_locks locks_paths) add_guard else add_guard


let apply_guard_release (guard_path : AccessPath.t) : t -> t =
  TSet.map (fun ({guards} as astate_el : t_element) : t_element ->
      {astate_el with guards= Guards.remove guard_path guards} )


let apply_guard_destroy (guard_path : AccessPath.t) : t -> t =
  Fn.compose (apply_guard_release guard_path) (apply_unlocks [guard_path])


let final_update : t -> t =
  let mapper ({atomic_calls; final_atomic_calls} as astate_el : t_element) : t_element =
    let final_atomic_calls : CallsSet.t ref = ref final_atomic_calls in
    AtomicCallsSet.iter
      (fun ((calls, _) : atomic_calls) : unit ->
        if CallSet.cardinal calls > 0 then
          final_atomic_calls := CallsSet.add calls !final_atomic_calls )
      atomic_calls ;
    {astate_el with atomic_calls= AtomicCallsSet.empty; final_atomic_calls= !final_atomic_calls}
  in
  Fn.compose reduce_atomic_calls (TSet.map mapper)

  (* ************************************ Memory accesses *********************************************** *)
  let is_memory_access_under_lock (astate: t) : bool =
  TSet.exists (fun {atomic_calls} ->
    AtomicCallsSet.exists (fun (_calls, lock) ->
      Lock.is_locked lock
    ) atomic_calls
  ) astate

  let is_struct_field_access (access_expr: HilExp.AccessExpression.t) : bool =
      match HilExp.AccessExpression.to_access_path access_expr with
      | (_base, accesses) ->
          List.exists accesses ~f:(function
            | AccessPath.FieldAccess _ -> true
            | _ -> false)

  let is_pointer (lhs: HilExp.AccessExpression.t) ~tenv : bool =
      match lhs with
      | HilExp.AccessExpression.Base (_, _) ->
          (* Retrieve the type of the variable using AccessExpression.get_typ *)
          (match HilExp.AccessExpression.get_typ lhs tenv with
           | Some typ ->
               Typ.is_pointer typ  (* Check if the type is a pointer type *)
           | None ->
               (* If no type found, assume it's not a pointer *)
               false)
      | _ ->
          (* Ignore other types of access expressions, such as fields or dereferences *)
          false

  let apply_lhs_memory_access ~(lhs: HilExp.AccessExpression.t) ~(loc: Location.t) ~(under_lock: bool) ~(tenv: Tenv.t) : t -> t =
      let is_field = is_struct_field_access lhs in

      let is_relevant_access lhs =
        match lhs with
        | HilExp.AccessExpression.Base (var, _) ->
            not (Var.is_return var)  (* Use the built-in is_return function *)
            && (Var.is_global var || is_pointer lhs ~tenv)
        | _ -> false
      in

      let mapper ({memory_accesses} as astate_el : t_element) : t_element =
        if is_relevant_access lhs then
          let memory_access = {
            MemoryAccess.access_path = lhs;
            location = loc;
            access_kind = Write;
            under_lock;
            is_struct_field = is_field;
          } in
          let memory_accesses = MemoryAccessSet.add memory_access memory_accesses in
          {astate_el with memory_accesses}
        else
          astate_el
      in
      TSet.map mapper

  let rec collect_access_expressions (e: HilExp.t) : HilExp.AccessExpression.t list =
      match e with
      | HilExp.AccessExpression access_expr -> [access_expr]
      | HilExp.Constant _ -> []
      | HilExp.Cast (_, subexp) -> collect_access_expressions subexp
      | HilExp.UnaryOperator (_, subexp, _) -> collect_access_expressions subexp
      | HilExp.BinaryOperator (_, e1, e2) ->
          collect_access_expressions e1 @ collect_access_expressions e2
      | HilExp.Closure _ -> []
      | HilExp.Sizeof _ -> []
      | HilExp.Exception _ -> []

    let apply_rhs_memory_access ~(rhs: HilExp.t) ~(loc: Location.t) ~(under_lock: bool) ~(tenv: Tenv.t) : t -> t =
      let access_exprs = collect_access_expressions rhs in
      let mapper ({memory_accesses} as astate_el : t_element) : t_element =
        let memory_accesses =
          List.fold access_exprs ~init:memory_accesses ~f:(fun acc access_expr ->
            let is_field = is_struct_field_access access_expr in
            let memory_access = {
              MemoryAccess.access_path = access_expr;
              location = loc;
              access_kind = Read;
              under_lock;
              is_struct_field = is_field;
            } in
            (* Check if the access expression is relevant (global or pointer) *)
            let should_add_access =
              match access_expr with
              | HilExp.AccessExpression.Base (var, _) ->
                  (* Track only if it's a global variable or pointer *)
                  (Var.is_global var) || (is_pointer access_expr ~tenv)
              | HilExp.AccessExpression.FieldOffset (ae, _) ->
                  is_pointer ae ~tenv  (* Check if the field's base is a pointer *)
              | HilExp.AccessExpression.ArrayOffset (ae, _, _) ->
                  is_pointer ae ~tenv  (* Check if the array's base is a pointer *)
              | HilExp.AccessExpression.AddressOf ae | HilExp.AccessExpression.Dereference ae ->
                  (* AddressOf and Dereference are usually associated with pointers *)
                  is_pointer ae ~tenv
            in
            if should_add_access then
              MemoryAccessSet.add memory_access acc  (* Add memory access if relevant *)
            else
              acc  (* Do nothing if the access is not relevant *)
          )
        in
        {astate_el with memory_accesses}
      in
      TSet.map mapper

(* ************************************ Operators *********************************************** *)

let leq ~(lhs : t) ~(rhs : t) : bool =
  (* The 'lhs' is less or equal to the 'rhs' if the 'lhs' is a subset of the 'rhs'. *)
  if phys_equal lhs rhs then true else TSet.subset lhs rhs


let join (astate1 : t) (astate2 : t) : t =
  (* Union of abstract states. *)
  if phys_equal astate1 astate2 then astate2 else reduce_atomic_calls (TSet.union astate1 astate2)


let widen ~(prev : t) ~(next : t) ~(num_iters : int) : t =
  (* Join the previous and next abstract states. *)
  if phys_equal prev next then next
  else if num_iters <= Config.atomic_sets_widen_limit then join prev next
  else next


(* ************************************ Summary ************************************************* *)

module Summary = struct
  type t =
  { atomic_calls: CallsSet.t
  ; calls: (CallSet.t list[@printer pp_calls])
  ; memory_accesses: MemoryAccessSet.t
  }
  [@@deriving compare, equal, show {with_path= false}]

  let create (astate : astate) : t =
    let atomic_calls : CallsSet.t ref = ref CallsSet.empty
    and s_calls : CallSet.t list ref = ref []
    and memory_accesses : MemoryAccessSet.t ref = ref MemoryAccessSet.empty
    in
    let iterator ({final_atomic_calls; calls; memory_accesses= mem_accs} : t_element) : unit =
      (* Merge atomic calls *)
      atomic_calls := CallsSet.union final_atomic_calls !atomic_calls ;

      (* Merge calls *)
      let merge_calls (i : int) (calls_i : CallSet.t) : unit =
        if not (CallSet.is_empty calls_i) then
          s_calls :=
            if Option.is_some (List.nth !s_calls i) then
              List.mapi !s_calls ~f:(fun (j : int) ->
                  if Int.equal i j then CallSet.union calls_i else Fn.id)
            else
              !s_calls @ [calls_i]
      in
      List.iteri calls ~f:merge_calls ;

      (* Merge memory accesses *)
      memory_accesses := MemoryAccessSet.union mem_accs !memory_accesses
    in
    TSet.iter iterator astate ;

    (* Build the summary *)
    { atomic_calls= !atomic_calls
    ; calls= !s_calls
    ; memory_accesses= !memory_accesses
    }

   let is_top_level_fun (pname : Procname.t) (summaries : (Procname.t * t) list) : bool =
      List.for_all summaries ~f:(fun (pname', {calls}) ->
        Procname.equal pname' pname ||
        List.for_all calls ~f:(fun callset ->
          not (List.exists ~f:(fun (call_name, _) ->
            String.equal (Procname.to_string pname) call_name
          ) (CallSet.elements callset))
        )
      )


  let print_atomic_sets ~(f_name : string) (oc : Out_channel.t) (summaries : (Procname.t * t) list)
      ({atomic_calls} : t) : int * int =
    let atomic_calls : CallsSet.t =
      let contains_call (call : call) : bool =
        List.exists summaries ~f:(fun ((_, {atomic_calls}) : Procname.t * t) : bool ->
            CallsSet.exists (CallSet.exists (is_same_call_diff_loc call)) atomic_calls )
      and mapper (calls : CallSet.t) : CallSet.t option =
        let calls : CallSet.t = CallSet.filter (Fn.compose Option.is_none snd) calls in
        if CallSet.is_empty calls then None else Some calls
      in
      if not Config.atomic_sets_ignore_single_atomic_calls then atomic_calls
      else Fn.compose (CallsSet.filter_map mapper) (reduce_calls_set ~contains_call) atomic_calls
    in
    if CallsSet.is_empty atomic_calls then (0, 0)
    else (
      Out_channel.fprintf oc "%s: " f_name ;
      let last_atomic_calls : CallSet.t option = CallsSet.max_elt_opt atomic_calls in
      let print_atomic_calls (calls : CallSet.t) : unit =
        Out_channel.fprintf oc "{%s}"
          (String.concat (List.map (CallSet.elements calls) ~f:fst) ~sep:", ") ;
        if not (CallSet.equal calls (Option.value_exn last_atomic_calls)) then
          Out_channel.output_char oc ' '
      in
      CallsSet.iter print_atomic_calls atomic_calls ;
      Out_channel.newline oc ;
      ( CallsSet.cardinal atomic_calls
      , CallsSet.fold
          (fun (calls : CallSet.t) : (int -> int) -> ( + ) (CallSet.cardinal calls))
          atomic_calls 0 ) )

     let print_memory_accesses (oc : Out_channel.t) (_summaries : (Procname.t * t) list)
        ({memory_accesses} : t) : unit =
      if MemoryAccessSet.is_empty memory_accesses then ()
      else (
        let accesses = MemoryAccessSet.elements memory_accesses in
        let location_to_string (loc : Location.t) : string =
          Format.asprintf "%a" Location.pp loc
        in
        let access_path_to_string (ap : HilExp.AccessExpression.t) : string =
          Format.asprintf "%a" HilExp.AccessExpression.pp ap
        in
        let access_kind_to_string (ak : MemoryAccess.access_kind) : string =
          match ak with
          | Read -> "Read"
          | Write -> "Write"
        in
        let print_access (access : MemoryAccess.t) : unit =
          Out_channel.fprintf oc
            "{ access_path: %s; location: %s; access_kind: %s; under_lock: %b; is_struct_field: %b }\n"
            (access_path_to_string access.access_path)
            (location_to_string access.location)
            (access_kind_to_string access.access_kind)
            access.under_lock
            access.is_struct_field
        in
        List.iter accesses ~f:print_access
      )
end

let apply_summary ({calls= s_calls; memory_accesses= s_memory_accesses} : Summary.t) : t -> t =
  if List.is_empty s_calls && MemoryAccessSet.is_empty s_memory_accesses then
    Fn.id
  else
    let mapper ({atomic_calls; calls; memory_accesses} as astate_el : t_element) : t_element =
      let calls : CallSet.t list ref = ref calls
      and joined_calls : CallSet.t ref = ref CallSet.empty in

      let iterator (i : int) (calls' : CallSet.t) : unit =
        if i < Config.atomic_sets_functions_depth_limit && not (CallSet.is_empty calls') then (
          joined_calls := CallSet.union calls' !joined_calls ;
          calls :=
            if Option.is_some (List.nth !calls (i + 1)) then
              List.mapi !calls ~f:(fun (j : int) ->
                  if Int.equal (i + 1) j then CallSet.union calls' else Fn.id)
            else
              !calls @ [calls']
        )
      in
      List.iteri s_calls ~f:iterator ;
      (* Handle memory accesses *)
      let memory_accesses =
        if is_memory_access_under_lock (TSet.singleton astate_el) then
          (* If under lock, mark all imported accesses as under_lock = true *)
          let locked_accesses =
            MemoryAccessSet.map (fun access ->
              { access with under_lock = true }
            ) s_memory_accesses
          in
          MemoryAccessSet.union memory_accesses locked_accesses
        else
          (* Otherwise, just merge normally *)
          MemoryAccessSet.union memory_accesses s_memory_accesses
      in
      let atomic_calls =
        AtomicCallsSet.map
          (fun ((calls, lock) : atomic_calls) : atomic_calls ->
            (CallSet.union calls !joined_calls, lock))
          atomic_calls
      in
      update_astate_el_after_calls {astate_el with atomic_calls; calls= !calls; memory_accesses}
    in
    Fn.compose reduce_atomic_calls (TSet.map mapper)
