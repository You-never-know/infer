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
  ; guards: Guards.t }
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
    ; guards= Guards.empty }


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
    let loc : Location.t option =
      if Config.atomic_sets_ignore_single_atomic_calls then Some loc else None
    in
    let atomic_calls : AtomicCallsSet.t =
      AtomicCallsSet.map
        (fun ((calls, lock) : atomic_calls) : atomic_calls ->
          (CallSet.add (f_name, loc) calls, lock) )
        atomic_calls
    and calls : CallSet.t list =
      List.mapi calls ~f:(fun (i : int) : (CallSet.t -> CallSet.t) ->
          if Int.equal i 0 then CallSet.add (f_name, loc) else Fn.id )
    in
    update_astate_el_after_calls {astate_el with atomic_calls; calls}
  in
  Fn.compose reduce_atomic_calls (TSet.map mapper)


let apply_locks (locks_paths : AccessPath.t list) : t -> t =
  let mapper ({guards; atomic_calls} as astate_el : t_element) : t_element =
    let locks_paths : AccessPath.t list = Guards.reveal_locks guards locks_paths in
    let atomic_calls : AtomicCallsSet.t =
      let fold (atomic_calls : AtomicCallsSet.t) (path : AccessPath.t) : AtomicCallsSet.t =
        let found : bool ref = ref false in
        let mapper ((calls, lock) as atomic_calls : atomic_calls) : atomic_calls =
          if AccessPath.equal path (Lock.path lock) then (
            found := true ;
            (calls, Lock.lock lock) )
          else atomic_calls
        in
        let atomic_calls : AtomicCallsSet.t = AtomicCallsSet.map mapper atomic_calls in
        if !found then atomic_calls
        else AtomicCallsSet.add (CallSet.empty, Lock.create path) atomic_calls
      in
      List.fold locks_paths ~init:atomic_calls ~f:fold
    in
    {astate_el with atomic_calls}
  in
  TSet.map mapper


let apply_unlocks (locks_paths : AccessPath.t list) : t -> t =
  let mapper ({guards; atomic_calls; final_atomic_calls} as astate_el : t_element) : t_element =
    let final_atomic_calls : CallsSet.t ref = ref final_atomic_calls
    and locks_paths : AccessPath.t list = Guards.reveal_locks guards locks_paths in
    let atomic_calls : AtomicCallsSet.t =
      let mapper ((calls, lock) as atomic_calls : atomic_calls) : atomic_calls option =
        let ((_, lock) as atomic_calls : atomic_calls) =
          if List.mem locks_paths (Lock.path lock) ~equal:AccessPath.equal then (
            let lock : Lock.t = Lock.unlock lock in
            if (not (Lock.is_locked lock)) && CallSet.cardinal calls > 0 then
              final_atomic_calls := CallsSet.add calls !final_atomic_calls ;
            (calls, lock) )
          else atomic_calls
        in
        if Lock.is_locked lock then Some atomic_calls else None
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
  type t = {atomic_calls: CallsSet.t; calls: (CallSet.t list[@printer pp_calls])}
  [@@deriving compare, equal, show {with_path= false}]

  let create (astate : astate) : t =
    let atomic_calls : CallsSet.t ref = ref CallsSet.empty
    and s_calls : CallSet.t list ref = ref [] in
    let iterator ({final_atomic_calls; calls} : t_element) : unit =
      atomic_calls := CallsSet.union final_atomic_calls !atomic_calls ;
      let iterator (i : int) (calls : CallSet.t) : unit =
        if not (CallSet.is_empty calls) then
          s_calls :=
            if Option.is_some (List.nth !s_calls i) then
              List.mapi !s_calls ~f:(fun (j : int) : (CallSet.t -> CallSet.t) ->
                  if Int.equal i j then CallSet.union calls else Fn.id )
            else !s_calls @ [calls]
      in
      List.iteri calls ~f:iterator
    in
    TSet.iter iterator astate ;
    {atomic_calls= !atomic_calls; calls= !s_calls}


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
end

let apply_summary ({calls= s_calls} : Summary.t) : t -> t =
  if List.is_empty s_calls then Fn.id
  else
    let mapper ({atomic_calls; calls} as astate_el : t_element) : t_element =
      let calls : CallSet.t list ref = ref calls
      and joined_calls : CallSet.t ref = ref CallSet.empty in
      let iterator (i : int) (calls' : CallSet.t) : unit =
        if i < Config.atomic_sets_functions_depth_limit && not (CallSet.is_empty calls') then (
          joined_calls := CallSet.union calls' !joined_calls ;
          calls :=
            if Option.is_some (List.nth !calls (i + 1)) then
              List.mapi !calls ~f:(fun (j : int) : (CallSet.t -> CallSet.t) ->
                  if Int.equal (i + 1) j then CallSet.union calls' else Fn.id )
            else !calls @ [calls'] )
      in
      List.iteri s_calls ~f:iterator ;
      let atomic_calls : AtomicCallsSet.t =
        AtomicCallsSet.map
          (fun ((calls, lock) : atomic_calls) : atomic_calls ->
            (CallSet.union calls !joined_calls, lock) )
          atomic_calls
      in
      update_astate_el_after_calls {astate_el with atomic_calls; calls= !calls}
    in
    Fn.compose reduce_atomic_calls (TSet.map mapper)
