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
module List = Caml.List


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
            issue           : IssueType.t (** Type of the issue used for identifing *)
}

module ProblemSet = Set.Make(struct
        type t = problem

        let compare a b = Location.compare a.loc b.loc
end)

let problemSetPrint fmt problem = F.fprintf fmt "On %a is a problem: %a with function %a (%a)" Location.pp problem.loc 
                                  String.pp problem.problemName String.pp problem.procName IssueType.pp problem.issue

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

let createLock lockName lockScore accessPath loc = {lockName = lockName; lockScore = lockScore; accessPath = accessPath; loc = loc} 

let addLock lock astate = let newElement = lock                               in 
                          let newPost    = LockSet.add newElement astate.post in 
                          {problems = astate.problems; post = newPost}
                                                  
let lockSetMember lock astate = LockSet.mem lock astate.post

let unlock2lock lockName = if String.equal lockName "urcu_memb_read_unlock" then "urcu_memb_read_lock"
                           else lockName

let increaseLockScore lock astate = let currentLock = LockSet.find lock astate.post                                                                      in
                                    let newLock     = createLock currentLock.lockName (currentLock.lockScore + 1) currentLock.accessPath currentLock.loc in
                                    let removedSet  = LockSet.remove currentLock astate.post                                                             in 
                                    let newPost     = LockSet.add newLock removedSet                                                                     in 
                                    {problems = astate.problems; post = newPost} 

let decreaseLockScore lock astate = let currentLock = LockSet.find lock astate.post                                                                      in
                                    let newLock     = createLock currentLock.lockName (currentLock.lockScore - 1) currentLock.accessPath currentLock.loc in
                                    let removedSet  = LockSet.remove currentLock astate.post                                                             in 
                                    let newPost     = LockSet.add newLock removedSet                                                                     in 
                                    {problems = astate.problems; post = newPost}                                     
                                 
    

let _printProblems fmt problems = ProblemSet.iter (problemSetPrint fmt) problems 


(** Operands *)
(*********************************************************** *)

let leq ~lhs ~rhs = ProblemSet.subset lhs.problems rhs.problems && LockSet.subset lhs.post rhs.post  

(** Unite sets *)
let join a b = let newProblems = ProblemSet.union a.problems b.problems in
               let newPost     = LockSet.union a.post b.post            in
               {problems = newProblems; post = newPost}
                
(** We join and join *)
let widen ~prev ~next ~num_iters:_ = join prev next


(** Other functions *)
(************************************************************************************* *)
                                 
(** Print lock name and score *)
let pp fmt astate = LockSet.iter (lockSetPrint fmt) astate.post (** Change to print all problems from the final summary *)

let hasViolation summary = not (ProblemSet.is_empty summary.problems)

module Summary = struct
                        
        let updateSummary astate summary = join astate summary     (** Updates a current summary with abstract state *)
end 

(** Add lock scores, and take the newer accessPath as well as the newer loc *)
let combineLocks (lockA : lockInfo) (lockB : lockInfo) = {lockName = lockB.lockName; lockScore = (lockA.lockScore + lockB.lockScore); 
                                                           loc = lockB.loc; accessPath = lockB.accessPath} 

(** check if the lock is a part of abstract state, if it is combine it with the one from summary, if not add it to the astate *)
(** Add check, if any problems have been resolved *)
let addLockFromSummary (lock : lockInfo) (astate : t) = if lockSetMember lock astate then 
                                                            let foundLock    = LockSet.find lock astate.post        in
                                                            let combinedLock = combineLocks foundLock lock          in 
                                                            let removedSet   = LockSet.remove lock astate.post      in
                                                            let newPost      = LockSet.add combinedLock removedSet  in
                                                            {problems = astate.problems; post = newPost}
                                                        else 
                                                            let newPost      = LockSet.add lock astate.post         in 
                                                            {problems = astate.problems; post = newPost} 
                        


let rec addElements list astate = if not (List.length list <> 0) then astate
                                   else 
                                       let firstElement = List.hd list                                   in
                                       let newAstate    = addLockFromSummary firstElement astate         in 
                                       let newList      = List.tl list                                   in 
                                       addElements newList newAstate 

let set2list set = LockSet.elements set 

(** Add scores of locks + maybe join problems, maybe problems will need to be joined manualy as well *)
let applySummary astate summary = if ProblemSet.is_empty summary.problems && LockSet.is_empty summary.post then astate
                                  else 
                                      let newProblemSet = ProblemSet.union astate.problems summary.problems in
                                      let summaryList   = set2list summary.post                             in
                                      let newAstate     = addElements summaryList astate                    in 
                                      {problems = newProblemSet; post = newAstate.post}


                                   
                              

type summary = t
