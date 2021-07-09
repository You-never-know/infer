(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module Set = Caml.Set

(*                                 Types                                               *)
(************************************************************************************* *)

type element = 
    { lock_name: string; 
      lock_score: int;
      access_path: AccessPath.t
    }


module LockSet = Set.Make (struct 
        type t = element
        let compare a b = String.compare a.lock_name b.lock_name 
    end)


type t = LockSet.t 

(** End info from every procedure, 
    1. preconditions -> conditions that need to be met 
    2. current_state -> state at the end of the procedure
    3. problem_loc -> line of code where is a problem located  
    4. lock_names -> set of locks that were encountered in code  
                
type astate = {         mutable preconditions   : (string, int) Hash.t ;
                        mutable current_state   : (string, int) Hash.t ;
                        mutable problem_loc     : (string, LocSet.t) Hash.t ;
                        mutable lock_names      : LockNames.t            }


type t = astate
*)
(*                              Initialization                                         *)
(************************************************************************************* *)
(** Keeps current state *)
let lock_info_hash_table = Hash.create (module String) 

(** Keeps info about preconditions *)
let prec = Hash.create (module String) 

(** Hash map of loc where a problem may be located *)
let problem_hash_table = Hash.create (module String) 

(** Keep names of found locks *)
let lock_name_set = LockNames.empty

(** General lock for output, in a case where a locked lock is needed and multiple locks are detected *)
let any_rcu_lock = "Any RCU lock"

(** Initial state of the analysis *)
let initial = {preconditions = prec; current_state = lock_info_hash_table; problem_loc = problem_hash_table; lock_names = lock_name_set}

(**                                 Set operations                                     *)
(************************************************************************************* *)
let add_lock_name name astate = LockNames.add name astate.lock_names 

(** Add iterators, will be needed in violation checks *)


(**                 Hash table operations  (name = lock_name)                          *)
(************************************************************************************* *)
(** true/false *)
let lock_in_current_state name astate = Hash.mem astate.current_state name  

let lock_in_preconditions name astate = Hash.mem astate.preconditions name  

(** Base.Hashtbl.add_exn checks for duplicates and does not add them to table *)
let add_lock_to_current_state name score astate = Hash.add_exn astate.current_state ~key:name ~data:score
                                                          
let get_lock_state name astate = Hash.find_exn astate.current_state name

let increment_lock_state name astate = Hash.set astate.current_state ~key:name ~data:((get_lock_state name astate) + 1)

let decrement_lock_state name astate = Hash.set astate.current_state ~key:name ~data:((get_lock_state name astate) - 1)

(** Base.Hashtbl.add checks for duplicates and does not add them to table *)
let add_precondition name score astate = Hash.add_exn astate.preconditions ~key:name ~data:score 

let remove_precondition name astate = Hash.remove astate.preconditions name

let get_precondition_state name astate = Hash.find_exn astate.preconditions name

(** If a state of precondition is met (state = 0), it can be removed from the hash table *)
let check_if_precondition_valid name astate = if (get_precondition_state name astate) <> 0 then true
                                              else false

(** Get set of loc associated with the given lock *)
let get_all_specified_loc name astate = Hash.find_exn astate.problem_loc name

let loc_in_table name loc astate = LocSet.mem loc (get_all_specified_loc name astate)  

(** If no problem detected for the lock, add new set for the lock,
    if a set already exists, add a new loc to it and replace it inside the hash map 
**)
let add_problem_loc name loc astate = 
    match (Hash.find astate.problem_loc name) with
    | Some set -> Hash.set astate.problem_loc ~key:name ~data:(LocSet.add loc set)
    | None -> Hash.add_exn astate.problem_loc ~key:name ~data:(LocSet.singleton loc)  

(** Problem if there are multiple problems with the same lock -> some of the problems may stay, multiple calls may be needed *)
let remove_problems_with_lock name astate = Hash.remove astate.problem_loc name


(*                              Operators                                              *)
(************************************************************************************* *)
(** TODO/maybe -> check if the right one has all elements from the left one *)
let leq ~lhs ~rhs = (Hash.length lhs.preconditions + Hash.length lhs.current_state + Hash.length lhs.problem_loc) <=  (Hash.length rhs.preconditions + Hash.length rhs.current_state + Hash.length rhs.problem_loc)


(** Merge hast tables and sets together *)
let join a b = Hash.merge_into ~src:a.current_state ~dst:b.current_state ~f:merge_function 
              (* let preconditions_merge = Hash.merge   a.preconditions b.preconditions ~f:merge_function             in
               let problem_loc_merge = Hash.merge     a.problem_loc   b.problem_loc   ~f:problem_loc_merge_function in
               let lock_names_merge = LockNames.union a.lock_names    b.lock_names                                  in 
               {preconditions = preconditions_merge; current_state = current_state_merge; problem_loc = problem_loc_merge; lock_names = lock_names_merge}   *)
                
(** *)
let widen ~prev ~next ~num_iters:_ = join prev next
(************************************************************************************* *)
                                 
(** TODO *)
let pp fmt astate = F.fprintf fmt "%d" astate.problem_loc

let rcu_lock locks_locked = locks_locked + 1

let rcu_unlock locks_locked = locks_locked - 1 

let has_violation locks_locked = locks_locked <> 0  

type summary = t
