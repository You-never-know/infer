(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module Hash = Caml.Hashtbl 
module Set = Caml.Set
module LockNames = Set.Make (String)
module List = Caml.List

(*                                 Types                                               *)
(************************************************************************************* *)

(** End info from every procedure, 
    1. preconditions -> conditions that need to be met 
    2. current_state -> state at the end of the procedure
    3. problem_loc -> line of code where is a problem located  
    4. lock_names -> set of locks that were encountered in code  
*)                 
type procedure_info = { mutable preconditions   : (string, int) Hash.t ;
                        mutable current_state   : (string, int) Hash.t ;
                        mutable problem_loc     : (string, int) Hash.t ;
                        mutable lock_names      : LockNames.t            }


type t = procedure_info

(*                              Initialization                                         *)
(************************************************************************************* *)
(** Keeps current state *)
let lock_info_hash_table = Hash.create 11

(** Keeps info about preconditions *)
let prec = Hash.create 5

(** Hash map of loc where a problem may be located *)
let problem_hash_table = Hash.create 11 

(** Keep names of found locks *)
let lock_name_set = LockNames.empty

(** General lock for output, in a case where a locked lock is needed and multiple locks are detected *)
let any_rcu_lock = "Any RCU lock"

(** Initial state of the analysis *)
let initial = {preconditions = prec; current_state = lock_info_hash_table; problem_loc = problem_hash_table; lock_names = lock_name_set}

(**                                 Set operations                                     *)
(************************************************************************************* *)
let add_lock_name name astate = LockNames.add name 

(** Add iterators, will be needed in violation checks *)


(**                 Hash table operations  (name = lock_name)                          *)
(************************************************************************************* *)
(** true/false *)
let lock_in_current_state name astate = Hash.mem astate.current_state name  

let lock_in_preconditions name astate = Hash.mem astate.preconditions name  

let add_lock_to_current_state name score astate = if not (lock_in_current_state name astate) 
                                                  then Hash.add astate.current_state name score
                                                          

let get_lock_state name astate = Hash.find astate.current_state name

let increment_lock_state name astate = Hash.replace astate.current_state name ( (get_lock_state name astate) + 1)

let decrement_lock_state name astate = Hash.replace astate.current_state name ( (get_lock_state name astate) - 1)

let add_precondition name score astate = if not (lock_in_preconditions name astate) then Hash.add astate.preconditions name score 

let remove_precondition name astate = Hash.remove astate.preconditions name

let get_precondition_state name astate = Hash.find astate.preconditions name

let check_if_precondition_valid name astate = if (get_precondition_state name astate) <> 0 then true
                                              else false

(** Get all loc for the given lock *)
let get_all_specified_loc name astate = Hash.find_all astate.problem_loc name

let loc_in_table name loc astate = List.mem loc (get_all_specified_loc name astate)  

let add_problem_loc name loc astate = if not (loc_in_table name loc astate) then Hash.add astate.problem_loc name loc

(** Problem if there are multiple problems with the same lock -> some of the problems may stay, multiple calls may be needed *)
let remove_problems_with_lock name astate = Hash.remove astate.problem_loc name


(*                              Operators                                              *)
(************************************************************************************* *)
(** *)
let leq ~lhs ~rhs = (Hash.length lhs.preconditions + Hash.length lhs.current_state + Hash.length lhs.problem_loc) <=  (Hash.length rhs.preconditions + Hash.length rhs.current_state + Hash.length rhs.problem_loc)

(** *)
let join a b = if a.current_state.lock_score > b.current_state.lock_score then a
               else b
(** *)
let widen ~prev ~next ~num_iters:_ = join prev next
(************************************************************************************* *)
                                 

let pp fmt astate = F.fprintf fmt "%d" astate.problem_loc

let rcu_lock locks_locked = locks_locked + 1

let rcu_unlock locks_locked = locks_locked - 1 

let has_violation locks_locked = locks_locked <> 0  

type summary = t
