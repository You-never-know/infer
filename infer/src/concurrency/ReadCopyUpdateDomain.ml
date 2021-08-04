(*
 * Copyright (c) 2020-present
 *
 * Daniel Marek (xmarek72@stud.fit.vutbr.cz)
 * Automated Analysis and Verification Research Group (VeriFIT)
 * Brno University of Technology, Czech Republic
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)


open! IStd
module F = Format
module Set = Caml.Set


(** Types *)
(*********************************************************** *)

type lockInfo = 
{             lockName    : string;         (** name of the lock *)
      mutable lockScore   : int;            (** score of the lock ("2" locked 2 times|"-2" twice unlocked *)
              accessPath  : AccessPath.base;(** access path of the lock *)
              loc         : Location.t      (** line of code where the lock was locked *)
}

module LockSet = Set.Make(struct
        type t = lockInfo

        let compare a b = String.compare a.lockName b.lockName 
end)

let lockSetPrint fmt lock = F.fprintf fmt "Lock %a on %a has score: %a" String.pp lock.lockName Location.pp lock.loc Int.pp lock.lockScore


type problem = 
{
            procName        : string; (** Name of the function where a problem was detected *)
            loc             : Location.t; (** Line of code, where the problem was detected *)
            problemName     : string;     (** Info about the problem *)
            issue           : IssueType.t (** Type of the issue *)
}

(* )
module ProblemSet = Set.Make(struct
        type t = problem

        let compare a b = Location.compare a.loc b.loc
end)
let problemSetPrint fmt problem = F.fprintf fmt "On line %a is a problem: %a with function %a (%a)" Location.pp problem.loc String.pp problem.problemName Procname.pp problem.procName IssueType.pp problem.issue
*)

type t = { (** abstract state *)
        precon: LockSet.t; (** Preconditions -> Lock scores + identifications that need to be met *)
        post: LockSet.t  (** State after the function *)
}

(** Init *)
(*********************************************************** *)

let initial = {
        precon = LockSet.empty;
        post = LockSet.empty
}
(* )
let initialProblems = ProblemSet.empty
*)

(** Abstract state functions *)
(*********************************************************** *)

let createLock lockName lockScore accessPath loc = Logging.progress "Computed post %a \n" Int.pp lockScore;{lockName = lockName; lockScore = lockScore; accessPath = accessPath; loc = loc} 

let addLock lock astate = let newElement = lock in 
                          let newPost = LockSet.add newElement astate.post in 
                          {precon = astate.precon; post = newPost}
                                                  
let member lock astate = LockSet.mem lock astate.post

let unlock2lock lockName = if String.equal lockName "urcu_memb_read_unlock" then "urcu_memb_read_lock"
                           else lockName

let increaseLockScore lock astate = let currentLock = LockSet.find lock astate.post in
                                    let newLock = createLock currentLock.lockName (currentLock.lockScore + 1) currentLock.accessPath currentLock.loc in
                                    let removedSet = LockSet.remove currentLock astate.post in 
                                    let newPost = LockSet.add newLock removedSet in 
                                    {precon = astate.precon; post = newPost} 

let decreaseLockScore lock astate = let currentLock = LockSet.find lock astate.post in
                                    let newLock = createLock currentLock.lockName (currentLock.lockScore - 1) currentLock.accessPath currentLock.loc in
                                    let removedSet = LockSet.remove currentLock astate.post in 
                                    let newPost = LockSet.add newLock removedSet in 
                                    {precon = astate.precon; post = newPost}                                     
                                 
    
(* )
let printProblems fmt problems = ProblemSet.iter (problemSetPrint fmt) problems 
*)

(** Operands *)
(*********************************************************** *)

let leq ~lhs ~rhs = LockSet.subset lhs.precon rhs.precon && LockSet.subset lhs.post rhs.post  

(** Unite sets *)
let join a b = let newPrecon = LockSet.union a.precon b.precon in
               let newPost   = LockSet.union a.post b.post     in
               {precon = newPrecon; post = newPost}
                
(** We join and join *)
let widen ~prev ~next ~num_iters:_ = join prev next
(************************************************************************************* *)
                                 
(** Print lock name and score *)
let pp fmt astate = LockSet.iter (lockSetPrint fmt) astate.post 

(** Check all lock scores 
let has_violation locks_locked = true  *)

type summary = t
