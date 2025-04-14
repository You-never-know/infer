(* Author: Dominik Harmim <iharmim@fit.vut.cz> *)

open! IStd
open AtomicityUtils
module F = Format
module L = Logging

(** Detection of atomicity violations domain implementation. *)

(* ************************************ Types and Modules *************************************** *)

(** A pair of function calls. *)
type calls_pair = string * string [@@deriving compare, equal, show {with_path= false}]

(** A set of pairs of function calls. *)
module CallsPairSet = MakePPSet (struct
  type t = calls_pair [@@deriving compare, equal, show {with_path= false}]
end)

(** A pair of atomic function calls. *)
type atomic_pair = calls_pair * Lock.t [@@deriving compare, equal, show {with_path= false}]

(** A set of pairs of atomic function calls. *)
module AtomicPairSet = struct
  include MakePPSet (struct
    type t = atomic_pair [@@deriving compare, equal, show {with_path= false}]
  end)

  let mem_pair (p : calls_pair) (s : t) : bool =
    exists (fun ((p', _lock) : atomic_pair) -> Tuple2.equal ~eq1:String.equal ~eq2:String.equal p p') s
end

(** A set of function calls. *)
module CallSet = MakePPSet (String)

(** A module that represents atomicity violations to be reported. *)
module Violations : sig
  include PrettyPrintable.PrintableEquatableOrderedType

  (** A severity of an atomicity violation to be reported. *)
  type severity = private
    | Warning  (** WARNING severity - used for local atomicity violations. *)
    | Error  (** ERROR severity - used for real (global) atomicity violations. *)
  [@@deriving compare, equal]

  val empty : t
  (** Creates an empty module. *)

  val is_empty : t -> bool
  (** Checks whether there are any violations to be reported. *)

  val add : calls_pair -> Location.t -> t -> t
  (** Adds a new violation to be reported. *)

  val union : t -> t -> t
  (** Makes the union of atomicity violations to be reported. *)

  val fold : f:(calls_pair * Location.t * severity -> 'a -> 'a) -> t -> 'a -> 'a
  (** [fold ~f:f s a] computes (f xN ... (f x2 (f x1 a))...), where x1 ... xN are the elements of s
      in increasing order. *)

  val make_all_warnings : t -> t
  (** Labels all atomicity violations to be reported as warnings. *)

  val severity_to_issue_type : severity -> IssueType.t
  (** Converts the severity of a violation to 'IssueType'. *)
end = struct
  type severity = Warning | Error [@@deriving compare, equal, show {with_path= false}]

  (** A structure that represents a single atomicity violation to be reported. *)
  type violation =
    {pair: calls_pair; loc: (Location.t[@printer Location.pp_file_pos]); severity: severity}
  [@@deriving compare, equal, show {with_path= false}]

  (** A set of atomicity violations to be reported. *)
  module ViolationSet = MakePPSet (struct
    type t = violation [@@deriving compare, equal, show {with_path= false}]
  end)

  type t = ViolationSet.t [@@deriving compare, equal, show {with_path= false}]

  let empty : t = ViolationSet.empty

  let is_empty : t -> bool = ViolationSet.is_empty

  let add (pair : calls_pair) (loc : Location.t) : t -> t =
    ViolationSet.add {pair; loc; severity= Error}

  let union : t -> t -> t = ViolationSet.union

  let fold ~(f : calls_pair * Location.t * severity -> 'a -> 'a) : t -> 'a -> 'a =
    ViolationSet.fold (fun ({pair; loc; severity} : violation) : ('a -> 'a) ->
        f (pair, loc, severity) )

  let make_all_warnings : t -> t =
    ViolationSet.map (fun (violation : violation) : violation -> {violation with severity= Warning})


  let severity_to_issue_type : severity -> IssueType.t = function
    | Warning ->
        IssueType.atomicity_violation_warning
    | Error ->
        IssueType.atomicity_violation_error
end

(* ************************************ Constants *********************************************** *)

(** An empty pair of function calls. *)
let empty_calls_pair : calls_pair = ("", "")

(* ************************************ Classes ************************************************* *)

(** A class that works with atomic pairs loaded from the first phase of the analysis. *)
class atomic_pairs =
  object (self)
    (** Is the class initialised? *)
    val mutable initialised : bool = false

    (** A set of pairs of function calls that should be called atomically. *)
    val mutable pairs : CallsPairSet.t = CallsPairSet.empty

    (** Initialises the class. *)
    method private init : unit =
      if not initialised then (
        initialised <- true ;
        (* Check the existence of the input file with atomic sets. *)
        ( match Sys.file_exists atomic_sets_file with
        | `Yes ->
            ()
        | _ ->
            L.die UserError
              "File '%s' does not exist. Run the detection of atomic sets first using \
               '--atomic-sets-only'."
              atomic_sets_file ) ;
        (* Read atomic pairs from the input file. *)
        let ic : In_channel.t = In_channel.create ~binary:false atomic_sets_file
        and read_line (l : string) : unit =
          if is_line_empty l then ()
          else
            let l : string = String.strip l in
            (* Truncate the function call name and split by atomic sets. *)
            let sets : string list =
              Str.split (Str.regexp "}[ \t]+{") (Str.replace_first (Str.regexp "^.+:[ \t]+") "" l)
            and iterator (set : string) : unit =
              (* Truncate parentheses and commas and split by function calls. *)
              let calls : string list =
                Str.split (Str.regexp ",[ \t]+")
                  (String.strip (Str.global_replace (Str.regexp "}\\|{") "" set))
              in
              let calls_count : int = List.length calls in
              if Int.equal calls_count 1 then
                pairs <- CallsPairSet.add ("", List.nth_exn calls 0) pairs
              else
                for i = 0 to calls_count - 2 do
                  for j = i + 1 to calls_count - 1 do
                    pairs <- CallsPairSet.add (List.nth_exn calls i, List.nth_exn calls j) pairs ;
                    pairs <- CallsPairSet.add (List.nth_exn calls j, List.nth_exn calls i) pairs
                  done
                done
            in
            List.iter sets ~f:iterator
        in
        In_channel.iter_lines ~fix_win_eol:true ic ~f:read_line ;
        In_channel.close ic )

   (** Checks whether a calls pair is violating atomicity. Returns true if it is a violation. *)
        method check_violating_atomicity_bool ?(check_first_empty : bool = false)
            ((_, psnd) as p : calls_pair) ~(atomic_last_pairs : AtomicPairSet.t) : bool =
          self#init ;
          let check (p : calls_pair) (check_first_empty : bool) : bool =
            let is_pair_locked (p : calls_pair) : bool =
              AtomicPairSet.fold
                (fun ((((_, psnd') as p'), _) : atomic_pair) acc ->
                  acc || equal_calls_pair p (if check_first_empty then ("", psnd') else p'))
                atomic_last_pairs false
            in
            CallsPairSet.mem p pairs && not (is_pair_locked p)
          in
          check p false || (check_first_empty && check ("", psnd) true)

  end

(** An instance of the 'atomic_pairs' class. *)
let atomic_pairs : atomic_pairs = new atomic_pairs

(* ************************************ Astate ************************************************** *)
let equal_pairs (p1a, p1b) (p2a, p2b) : bool =
    let comparison1 = String.equal p1a p2a in
    let comparison2 = String.equal p1b p2b in
    let comparison3 = String.equal p1a p2b in
    let comparison4 = String.equal p1b p2a in
    (comparison1 && comparison2) || (comparison3 && comparison4)

(* Type to store correct usage locations *)
type correct_atomic_usage = calls_pair * Location.t
[@@deriving compare, equal, show]

module CorrectAtomicUsageOrd = struct
  type t = calls_pair * Location.t

  let compare ((pair1, loc1) : t) ((pair2, loc2) : t) : int =
    (* Compare by the pair first *)
    let cmp_pairs = String.compare (fst pair1) (fst pair2) in
    if cmp_pairs <> 0 then
      cmp_pairs
    else
      (* If pairs are equal, compare by the location *)
      Location.compare loc1 loc2

  let pp fmt ((pair, loc) : t) =
    Format.fprintf fmt "%a (at %a)"
      pp_calls_pair pair
      Location.pp loc
end


module CorrectAtomicUsageSet = MakePPSet(CorrectAtomicUsageOrd)
type correct_atomic_usage_set = CorrectAtomicUsageSet.t

let add_to_correct_usage_set (pair : calls_pair) (loc : Location.t) (set : correct_atomic_usage_set) : correct_atomic_usage_set =
  let found = CorrectAtomicUsageSet.fold
    (fun (existing_pair, existing_loc) acc ->
      if equal_pairs existing_pair pair && Location.equal existing_loc loc then Some (existing_pair, existing_loc) else acc
    )
    set None
  in
  match found with
  | Some (existing_pair, existing_loc) ->
      (* Location already exists, no need to add it again *)
      set
  | None ->
      CorrectAtomicUsageSet.add (pair, loc) set

(** An element of an abstract state. *)
type t_element =
  { first_call: string
  ; last_pair: calls_pair
  ; nested_last_calls: CallSet.t
  ; violations: Violations.t
  ; correct_usages: CorrectAtomicUsageSet.t
  ; atomic_last_pairs: AtomicPairSet.t
  ; guards: Guards.t
  ; calls: CallSet.t
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
    (Config.atomicity_violations_widen_limit > 0)
    "Input argument '--atomicity-violations-widen-limit' should be greater than 0, %d given."
    Config.atomicity_violations_widen_limit ;
  (* An initial abstract state is a set with a single empty element. *)
  TSet.singleton
    { first_call= ""
    ; last_pair= empty_calls_pair
    ; nested_last_calls= CallSet.empty
    ; violations= Violations.empty
    ; correct_usages= CorrectAtomicUsageSet.empty
    ; atomic_last_pairs= AtomicPairSet.empty
    ; guards= Guards.empty
    ; calls= CallSet.empty }

let apply_call ~(f_name : string) (loc : Location.t) : t -> t =
  let mapper (astate_el : t_element) : t_element =
    let calls_pair_push : calls_pair -> string -> calls_pair = Fn.compose Tuple2.create snd in
    let first_call : string =
      if String.is_empty astate_el.first_call then f_name else astate_el.first_call
    and last_pair : calls_pair = calls_pair_push astate_el.last_pair f_name
    and violations : Violations.t ref = ref astate_el.violations
    and correct_usages : CorrectAtomicUsageSet.t ref = ref astate_el.correct_usages
    and atomic_last_pairs : AtomicPairSet.t =
      AtomicPairSet.map
        (fun ((p, lock) : atomic_pair) : atomic_pair -> (calls_pair_push p f_name, lock))
        astate_el.atomic_last_pairs
    and calls : CallSet.t = CallSet.add f_name astate_el.calls
    in

    (* Check last_pair for violation or correct usage *)
    if atomic_pairs#check_violating_atomicity_bool last_pair ~atomic_last_pairs ~check_first_empty:true then
      violations := Violations.add last_pair loc !violations
    else if AtomicPairSet.mem_pair last_pair atomic_last_pairs then
      correct_usages := add_to_correct_usage_set last_pair loc !correct_usages ;

    let iterator (last_call : string) : unit =
      let p : calls_pair = (last_call, f_name) in
      let atomic_last_pairs : AtomicPairSet.t =
        AtomicPairSet.map
          (fun ((((pfst, _) as p'), lock) : atomic_pair) : atomic_pair ->
            ((if String.is_empty pfst then p' else p), lock) )
          atomic_last_pairs
      in
      if atomic_pairs#check_violating_atomicity_bool p ~atomic_last_pairs ~check_first_empty:false then
        violations := Violations.add p loc !violations
      else if AtomicPairSet.mem_pair p atomic_last_pairs then
        correct_usages := add_to_correct_usage_set p loc !correct_usages
    in
    CallSet.iter iterator astate_el.nested_last_calls ;
    { astate_el with
      first_call
    ; last_pair
    ; nested_last_calls = CallSet.empty
    ; violations = !violations
    ; correct_usages = !correct_usages
    ; atomic_last_pairs
    ; calls }
  in
  TSet.map mapper

let apply_locks (locks_paths : AccessPath.t list) : t -> t =
  let mapper ({guards; atomic_last_pairs} as astate_el : t_element) : t_element =
    let locks_paths : AccessPath.t list = Guards.reveal_locks guards locks_paths in
    let atomic_last_pairs : AtomicPairSet.t =
      let fold (atomic_last_pairs : AtomicPairSet.t) (path : AccessPath.t) : AtomicPairSet.t =
        let found : bool ref = ref false in
        let mapper ((p, lock) as atomic_pair : atomic_pair) : atomic_pair =
          if AccessPath.equal path (Lock.path lock) then (
            found := true ;
            (p, Lock.lock lock) )
          else atomic_pair
        in
        let atomic_last_pairs : AtomicPairSet.t = AtomicPairSet.map mapper atomic_last_pairs in
        if !found then atomic_last_pairs
        else AtomicPairSet.add (empty_calls_pair, Lock.create path) atomic_last_pairs
      in
      List.fold locks_paths ~init:atomic_last_pairs ~f:fold
    in
    {astate_el with atomic_last_pairs}
  in
  TSet.map mapper


let apply_unlocks (locks_paths : AccessPath.t list) : t -> t =
  let mapper ({guards; atomic_last_pairs} as astate_el : t_element) : t_element =
    let locks_paths : AccessPath.t list = Guards.reveal_locks guards locks_paths in
    let atomic_last_pairs : AtomicPairSet.t =
      let mapper ((p, lock) as atomic_pair : atomic_pair) : atomic_pair option =
        let ((_, lock) as atomic_pair : atomic_pair) =
          if List.mem locks_paths (Lock.path lock) ~equal:AccessPath.equal then (p, Lock.unlock lock)
          else atomic_pair
        in
        if Lock.is_locked lock then Some atomic_pair else None
      in
      AtomicPairSet.filter_map mapper atomic_last_pairs
    in
    {astate_el with atomic_last_pairs}
  in
  TSet.map mapper

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


(* ************************************ Operators *********************************************** *)

let leq ~(lhs : t) ~(rhs : t) : bool =
  (* The 'lhs' is less or equal to the 'rhs' if the 'lhs' is a subset of the 'rhs'. *)
  if phys_equal lhs rhs then true else TSet.subset lhs rhs

 let join (astate1 : t) (astate2 : t) : t =
  (* Union of abstract states. *)
  if phys_equal astate1 astate2 then astate2
  else TSet.union astate1 astate2

let widen ~(prev : t) ~(next : t) ~(num_iters : int) : t =
  (* Join the previous and next abstract states. *)
  if phys_equal prev next then next
  else if num_iters <= Config.atomicity_violations_widen_limit then join prev next
  else next

(* ************************************ Summary ************************************************* *)

module Summary = struct
  type t =
    { first_calls: CallSet.t
    ; last_calls: CallSet.t
    ; violations: Violations.t
    ; correct_usages: CorrectAtomicUsageSet.t
    ; calls: CallSet.t }
  [@@deriving compare, equal, show {with_path= false}]

(* ************************************ Violation count ************************************************* *)

  type violation_count = calls_pair * int
  [@@deriving compare, equal, show]

    let compare_violation_count (v1 : violation_count) (v2 : violation_count) : int =
      let (pair1, count1) = v1 in
      let (pair2, count2) = v2 in
      (* First compare by the violation pair *)
      let cmp_pairs =
        if equal_pairs pair1 pair2 then 0
        else
          (* If pairs are not equal, determine the comparison result based on their lexicographic ordering *)
          String.compare (fst pair1) (fst pair2)
      in
      if Int.equal cmp_pairs 0 then
        (* If pairs are equal, compare by count *)
        Int.compare count1 count2
      else
        cmp_pairs

   module ViolationCountOrd = struct
      type t = violation_count
      let compare = compare_violation_count

      let pp fmt (pair, count) =
        Format.fprintf fmt "%a (count: %d)" pp_calls_pair pair count
    end

    (* Define the ViolationCountSet after ViolationCountOrd is defined *)
    module ViolationCountSet = MakePPSet(ViolationCountOrd)

    type violation_count_set = ViolationCountSet.t

    let violation_set_find_pair_and_get_count (pair : calls_pair) (set : violation_count_set) : int option =
      let result = ViolationCountSet.fold
        (fun (existing_pair, count) acc ->
          if equal_pairs existing_pair pair then
            Some count  (* If the pair matches, return the count *)
          else
            acc         (* Otherwise, continue searching *)
        )
        set
        None  (* Initial accumulator is None, which means no pair found *)
      in
      result

(* ************************************ Correct usage count ************************************************* *)

type correct_usage_count = calls_pair * int
[@@deriving compare, equal, show]

let compare_correct_usage_count (c1 : correct_usage_count) (c2 : correct_usage_count) : int =
  let (pair1, count1) = c1 in
  let (pair2, count2) = c2 in
  (* First compare by the usage pair *)
  let cmp_pairs =
    if equal_pairs pair1 pair2 then 0
    else
      (* If pairs are not equal, determine the comparison result based on their lexicographic ordering *)
      String.compare (fst pair1) (fst pair2)
  in
  if Int.equal cmp_pairs 0 then
    (* If pairs are equal, compare by count *)
    Int.compare count1 count2
  else
    cmp_pairs

module CorrectUsageCountOrd = struct
  type t = correct_usage_count
  let compare = compare_correct_usage_count

  let pp fmt (pair, count) =
    Format.fprintf fmt "%a (count: %d)" pp_calls_pair pair count
end

(* Define the CorrectUsageCountSet after CorrectUsageCountOrd is defined *)
module CorrectUsageCountSet = MakePPSet(CorrectUsageCountOrd)
type correct_usage_count_set = CorrectUsageCountSet.t

let correct_usage_set_find_pair_and_get_count (pair : calls_pair) (set : correct_usage_count_set) : int option =
  let result = CorrectUsageCountSet.fold
    (fun (existing_pair, count) acc ->
      if equal_pairs existing_pair pair then
        Some count  (* If the pair matches, return the count *)
      else
        acc         (* Otherwise, continue searching *)
    )
    set
    None  (* Initial accumulator is None, which means no pair found *)
  in
  result

(* ************************************ Summary functions ************************************************* *)
let create (astate : astate) : t =
  let first_calls : CallSet.t ref = ref CallSet.empty
  and last_calls : CallSet.t ref = ref CallSet.empty
  and s_violations : Violations.t ref = ref Violations.empty
  and s_correct_usages : CorrectAtomicUsageSet.t ref = ref CorrectAtomicUsageSet.empty
  and s_calls : CallSet.t ref = ref CallSet.empty in

  let iterator ({first_call; last_pair; violations; correct_usages; calls} : t_element) : unit =
    if not (String.is_empty first_call) then first_calls := CallSet.add first_call !first_calls ;
    if not (String.is_empty (snd last_pair)) then
      last_calls := CallSet.add (snd last_pair) !last_calls ;
    s_violations := Violations.union violations !s_violations ;
    s_correct_usages := CorrectAtomicUsageSet.union correct_usages !s_correct_usages ;
    s_calls := CallSet.union calls !s_calls
  in
  TSet.iter iterator astate ;
  { first_calls= !first_calls
  ; last_calls= !last_calls
  ; violations= !s_violations
  ; correct_usages= !s_correct_usages
  ; calls= !s_calls }

  let is_top_level_fun (pname : Procname.t) : (Procname.t * t) list -> bool =
    List.for_all ~f:(fun ((pname' : Procname.t), ({calls} : t)) : bool ->
        Procname.equal pname' pname || not (CallSet.mem (Procname.to_string pname) calls) )

 (* ************************************ Hard limit functions ************************************************* *)

  let iterate_over_violations_in_summary (summary : t) (violationCountSet : violation_count_set) : violation_count_set =
      let {violations} = summary in
      Violations.fold
        ~f:(fun (pair, _loc, _severity) acc ->
          (* Only count by the pair, disregarding loc and severity *)
          match violation_set_find_pair_and_get_count pair acc with
          | Some count ->
              let updated_set = ViolationCountSet.remove (pair, count) acc in
              ViolationCountSet.add (pair, count + 1) updated_set
          | None ->
              ViolationCountSet.add (pair, 1) acc
        )
        violations
        violationCountSet

  let count_violations (summaries : (Procname.t * t) list) : violation_count_set =
      List.fold_left
        ~f:(fun acc (proc, summary) ->
            if not (is_top_level_fun proc summaries) then acc
            else iterate_over_violations_in_summary summary acc
        )
        ~init:ViolationCountSet.empty
        summaries

 let filter_summary_hard_limit (violationCountSet : violation_count_set) (summary : Procname.t * t) : Procname.t * t =
  let proc, state = summary in
  (* Print out the full state of the summary *)
(* Format.printf "@[<v>Summary for procedure: %a@]@." Procname.pp proc; *)
(* Format.printf "@[<v>Violations: %a@]@." Violations.pp state.violations; *)
(* Format.printf "@[<v>Correct usage counts: %a@]@." CorrectUsageCountSet.pp state.correct_usage_counts; *)
(* Format.printf "@[<v>Calls: %a@]@." CallSet.pp state.calls; *)
(* Format.print_flush (); *)
  let new_violations =
    Violations.fold
      ~f:(fun (pair, loc, _) acc ->
        match violation_set_find_pair_and_get_count pair violationCountSet with
        | Some count when count >= Config.atomicity_violation_min_limit_to_print ->
            Violations.add pair loc acc
        | _ -> acc)
      state.violations
      Violations.empty
  in
  (proc, {state with violations = new_violations})

(* ************************************ Relative limit functions ************************************************* *)

let iterate_over_correct_usages_in_summary
    (summary : t)
    (correctUsageCountSet : correct_usage_count_set)
    : correct_usage_count_set =
  let {correct_usages} = summary in
  CorrectAtomicUsageSet.fold
    (fun (pair, _loc) acc ->
      match correct_usage_set_find_pair_and_get_count pair acc with
      | Some count ->
          let updated_set = CorrectUsageCountSet.remove (pair, count) acc in
          CorrectUsageCountSet.add (pair, count + 1) updated_set
      | None ->
          CorrectUsageCountSet.add (pair, 1) acc
    )
    correct_usages
    correctUsageCountSet

let count_correct_usages (summaries : (Procname.t * t) list) : correct_usage_count_set =
  List.fold_left
    ~f:(fun acc (proc, summary) ->
      if not (is_top_level_fun proc summaries) then acc
      else iterate_over_correct_usages_in_summary summary acc
    )
    ~init:CorrectUsageCountSet.empty
    summaries

  let passed_relative_limit (violation_count : int option) (correct_usage_counts : int option) : bool =
    match violation_count with
        | None -> false (* There are no atomic pair violations, it should not get reported *)
        | Some v_count ->
            match correct_usage_counts with
                | None -> true (* There are only incorrect usages of atomic pairs, it should definitely be reported *)
                | Some c_count ->
                    let min_percentage = Config.atomicity_violation_min_percentage_of_atomicity_violations_for_pairs_to_print in
                    let total_count = v_count + c_count in
                    let violation_percentage = int_of_float ((float_of_int v_count /. float_of_int total_count) *. 100.0) in
(*                   Format.printf "Violation count: %d, Correct count: %d, Total: %d, Violation %%: %d%%, Min required %%: %d%%@." *)
(*                        v_count c_count total_count violation_percentage min_percentage; *)
(*                   Format.print_flush (); *)
                    violation_percentage >= min_percentage

  let filter_summary_relative_limit (violationCountSet : violation_count_set) (correctUsageSet : correct_usage_count_set)
                                    (summary : Procname.t * t) : Procname.t * t =
    let proc, state = summary in
(*     Format.printf "@[<v>Summary for procedure: %a@]@." Procname.pp proc; *)
(*     Format.printf "@[<v>Violations: %a@]@." Violations.pp state.violations; *)
(*     Format.printf "@[<v>Correct atomic usage set: %a@]@." CorrectAtomicUsageSet.pp state.correct_usages; *)
(*     Format.printf "@[<v>Calls: %a@]@." CallSet.pp state.calls; *)
(*     Format.print_flush (); *)
    let new_violations =
      Violations.fold
          ~f:(fun (pair, loc, _) acc ->
            let violation_count = violation_set_find_pair_and_get_count pair violationCountSet in
            let correct_usage_count = correct_usage_set_find_pair_and_get_count pair correctUsageSet in
            if passed_relative_limit violation_count correct_usage_count then
                Violations.add pair loc acc
            else
                acc
          )
          state.violations
          Violations.empty
    in
    (proc, {state with violations = new_violations})

  let filter_summaries (summaries : (Procname.t * t) list) =
      if Config.atomicity_violation_min_limit_to_print <= 1
         && Config.atomicity_violation_min_percentage_of_atomicity_violations_for_pairs_to_print <= 0 then
        summaries
      else
        let violation_counts = count_violations summaries in
        let summaries_after_hard_filter =
          if Config.atomicity_violation_min_limit_to_print > 1 then
            List.map summaries ~f:(filter_summary_hard_limit violation_counts)
          else
            summaries
        in
        let correct_usage_counts = count_correct_usages summaries in
        let summaries_after_relative_filter =
          if Config.atomicity_violation_min_percentage_of_atomicity_violations_for_pairs_to_print > 0 then
            List.map summaries_after_hard_filter ~f:(filter_summary_relative_limit violation_counts correct_usage_counts)
          else
            summaries_after_hard_filter
        in
        summaries_after_relative_filter

  let report_atomicity_violations
      ~(f : Location.t -> msg:string -> IssueType.t -> IssueLog.t -> IssueLog.t) ({violations} : t)
      : IssueLog.t -> IssueLog.t =
    (* Report atomicity violations from atomicity violations stored in the summary. *)
    let fold (((pfst, psnd) : calls_pair), (loc : Location.t), (severity : Violations.severity))
        (issue_log : IssueLog.t) : IssueLog.t =
      if String.is_empty pfst && String.is_empty psnd then issue_log
      else
        let msg : string =
          let warning_msg : string =
            match severity with Warning -> " within a Current Function" | _ -> ""
          in
          if (not (String.is_empty pfst)) && not (String.is_empty psnd) then
            F.asprintf
              "Atomicity Violation%s! - Functions '%s' and '%s' should be called atomically."
              warning_msg pfst psnd
          else
            F.asprintf "Atomicity Violation%s! - Function '%s' should be called atomically."
              warning_msg
              (if String.is_empty pfst then psnd else pfst)
        in
        f loc ~msg (Violations.severity_to_issue_type severity) issue_log
    in
    Violations.fold ~f:fold violations
end

let apply_summary
    ({first_calls; last_calls; violations = s_violations; correct_usages = s_correct_usages} : Summary.t)
    (loc : Location.t) : t -> t =
  if CallSet.is_empty first_calls && CallSet.is_empty last_calls && Violations.is_empty s_violations
  then Fn.id
  else
    let mapper ({last_pair; atomic_last_pairs; violations; correct_usages} as astate_el : t_element) : t_element =
      let violations : Violations.t ref =
        let summary_violations : Violations.t =
          if AtomicPairSet.is_empty atomic_last_pairs then s_violations
          else Violations.make_all_warnings s_violations
        in
        ref (Violations.union violations summary_violations)
      in
      let correct_usages : CorrectAtomicUsageSet.t ref =
        ref (CorrectAtomicUsageSet.union correct_usages s_correct_usages)
      in
      let last_call : string = snd last_pair in
      let iterator (first_call : string) : unit =
        let p : calls_pair = (last_call, first_call) in
        let atomic_last_pairs_mapped : AtomicPairSet.t =
          AtomicPairSet.map (Fn.compose (Tuple2.create p) snd) atomic_last_pairs
        in
        let is_violation =
          atomic_pairs#check_violating_atomicity_bool p ~atomic_last_pairs:atomic_last_pairs_mapped
        in
        if is_violation then
          violations := Violations.add p loc !violations
        else if AtomicPairSet.mem_pair p atomic_last_pairs_mapped then
          correct_usages := add_to_correct_usage_set p loc !correct_usages
      in
      CallSet.iter iterator first_calls ;
      { astate_el with
        nested_last_calls = last_calls;
        violations = !violations;
        correct_usages = !correct_usages }
    in
    TSet.map mapper
