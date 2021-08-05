(*
 * Copyright (c) 2020-present
 *
 * Daniel Marek (xmarek72@stud.fit.vutbr.cz)
 * Prof. Ing. Tomáš Vojnar Ph.D. (vojnar@fit.vut.cz)
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
            lockNeeded      : bool;       (** In a case of rcu_dereference, when no lock has been used yet, or multiple locks are used and not one is locked, if true -> false this probme is removed *)
            problemLock     : lockInfo;   (** Info about a problematic lock, in a case of lock score going to 0, problem is removed *)
            procName        : string;     (** Name of the function where a problem was detected *)
            loc             : Location.t; (** Line of code, where the problem was detected *)
            problemName     : string;     (** Info about the problem *)
            issue           : IssueType.t (** Type of the issue *)
}

module ProblemSet = Set.Make(struct
        type t = problem

        let compare a b = Location.compare a.loc b.loc
end)
(* )
let problemSetPrint fmt problem = F.fprintf fmt "On %a is a problem: %a with function %a (%a)" Location.pp problem.loc String.pp problem.problemName String.pp problem.procName IssueType.pp problem.issue
*)

type astate = { (** abstract state *)
        problems: ProblemSet.t; (** Problems encountered *)
        post    : LockSet.t     (** State after the function *)
}

type t = astate

(** Init *)
(*********************************************************** *)

let initial = {
        problems = ProblemSet.empty;
        post     = LockSet.empty
}

(** Abstract state functions *)
(*********************************************************** *)

let createLock lockName lockScore accessPath loc = Logging.progress "Computed post %a \n" Int.pp lockScore;{lockName = lockName; lockScore = lockScore; accessPath = accessPath; loc = loc} 

let addLock lock astate = let newElement = lock in 
                          let newPost = LockSet.add newElement astate.post in 
                          {problems = astate.problems; post = newPost}
                                                  
let member lock astate = LockSet.mem lock astate.post

let unlock2lock lockName = if String.equal lockName "urcu_memb_read_unlock" then "urcu_memb_read_lock"
                           else lockName

let increaseLockScore lock astate = let currentLock = LockSet.find lock astate.post in
                                    let newLock = createLock currentLock.lockName (currentLock.lockScore + 1) currentLock.accessPath currentLock.loc in
                                    let removedSet = LockSet.remove currentLock astate.post in 
                                    let newPost = LockSet.add newLock removedSet in 
                                    {problems = astate.problems; post = newPost} 

let decreaseLockScore lock astate = let currentLock = LockSet.find lock astate.post in
                                    let newLock = createLock currentLock.lockName (currentLock.lockScore - 1) currentLock.accessPath currentLock.loc in
                                    let removedSet = LockSet.remove currentLock astate.post in 
                                    let newPost = LockSet.add newLock removedSet in 
                                    {problems = astate.problems; post = newPost}                                     
                                 
    
(* )
let printProblems fmt problems = ProblemSet.iter (problemSetPrint fmt) problems 
*)

(** Operands *)
(*********************************************************** *)

let leq ~lhs ~rhs = ProblemSet.subset lhs.problems rhs.problems && LockSet.subset lhs.post rhs.post  

(** Unite sets *)
let join a b = let newProblems = ProblemSet.union a.problems b.problems in
               let newPost   = LockSet.union a.post b.post              in
               {problems = newProblems; post = newPost}
                
(** We join and join *)
let widen ~prev ~next ~num_iters:_ = join prev next
(************************************************************************************* *)
                                 
(** Print lock name and score *)
let pp fmt astate = LockSet.iter (lockSetPrint fmt) astate.post 

module Summary = struct
        (** Write a function, that will join post from astate and the current summary, and choose problems that will be added to the summary *)

        let makeSummary astate = astate (** Create the initial summary *)
        let updateSummary _astate summary = summary (** Update the current summary with the current abstract state *)
        let makeFinalSummary astate summary = join astate summary (** Take all the problems and lock states and joing them together *)
end 

type summary = t
