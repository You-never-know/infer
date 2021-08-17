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

(** Set functions *)

let set2list set = LockSet.elements set 

let lockSetMember lock astate = LockSet.mem lock astate.post

(** Choose the lock with the higher score *)
let chooseLock (lockA : lockInfo) (lockB : lockInfo) = let newLockScore = max lockA.lockScore lockB.lockScore in 
                                                       {lockName = lockB.lockName; lockScore = newLockScore; 
                                                       loc = lockB.loc; accessPath = lockB.accessPath} 

(** Add lock scores, and take the newer accessPath as well as the newer loc *)
let combineLocks (lockA : lockInfo) (lockB : lockInfo) = {lockName = lockB.lockName; lockScore = (lockA.lockScore + lockB.lockScore); 
                                                          loc = lockB.loc; accessPath = lockB.accessPath} 


(** check if the lock is a part of abstract state, if it is combine it with the one from summary, if not add it to the astate *)
let joinLockWithAstate (lock : lockInfo) (astate : t) = if lockSetMember lock astate then 
                                                             let foundLock    = LockSet.find lock astate.post        in
                                                             let combinedLock = chooseLock foundLock lock            in 
                                                             let removedSet   = LockSet.remove lock astate.post      in
                                                             let newPost      = LockSet.add combinedLock removedSet  in
                                                             {problems = astate.problems; post = newPost}
                                                         else 
                                                             let newPost      = LockSet.add lock astate.post         in 
                                                             {problems = astate.problems; post = newPost} 
                        



(** check if the lock is a part of abstract state, if it is combine it with the one from summary, if not add it to the astate *)
let joinLockWithSummary (lock : lockInfo) (astate : t) = if lockSetMember lock astate then 
                                                             let foundLock    = LockSet.find lock astate.post        in
                                                             let combinedLock = combineLocks foundLock lock          in 
                                                             let removedSet   = LockSet.remove lock astate.post      in
                                                             let newPost      = LockSet.add combinedLock removedSet  in
                                                             {problems = astate.problems; post = newPost}
                                                         else 
                                                             let newPost      = LockSet.add lock astate.post         in 
                                                             {problems = astate.problems; post = newPost} 
                        

(** Add the scores of locks in astate  *)
let rec addElements list astate = if not (List.length list <> 0) then astate
                                  else 
                                       let firstElement = List.hd list                                   in
                                       let newAstate    = joinLockWithSummary firstElement astate        in 
                                       let newList      = List.tl list                                   in 
                                       addElements newList newAstate 

(** Chose a score of the final lock from the two given *)
let rec joinElements list astate = if not (List.length list <> 0) then astate
                                   else 
                                       let firstElement = List.hd list                                   in
                                       let newAstate    = joinLockWithAstate firstElement astate         in 
                                       let newList      = List.tl list                                   in 
                                       joinElements newList newAstate 



(** LockSet functions *)

let createLock lockName lockScore accessPath loc = {lockName = lockName; lockScore = lockScore; accessPath = accessPath; loc = loc} 

let addLock lock astate = let newElement = lock                               in 
                          let newPost    = LockSet.add newElement astate.post in 
                          {problems = astate.problems; post = newPost}
                                                  

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

(** ProblemSet functions *)                               

let checkIfLocked lock = if lock.lockScore > 0 then true
                         else false

let rec findLock list = if not (List.length list <> 0) then None 
                        else 
                            let firstElement = List.hd list                       in
                            if checkIfLocked firstElement then Some firstElement
                            else 
                                let newList  = List.tl list                       in 
                                findLock newList  


let findProblemLock astate = let lockList = set2list astate.post in
                             findLock lockList
                             

let addSynchronizationProblem ~problemLock ~procName ~loc astate = 
                let newProblem    = {lockNeeded = false; problemLock = problemLock; 
                                     procName = procName; loc = loc;
                                     problemName = "Deadlock -> synchronization inside a critical section"; 
                                     issue = IssueType.rcu_synchronization_problem}                             in                  
                                                                                         
                let newProblemSet = ProblemSet.add newProblem astate.problems                                   in
                {problems = newProblemSet; post = astate.post} 


(** Print functions *)

let problemSetPrint fmt problem = F.fprintf fmt "On %a is a problem: %a with function %a (%a)" Location.pp problem.loc 
                                  String.pp problem.problemName String.pp problem.procName IssueType.pp problem.issue

let report {InterproceduralAnalysis.proc_desc; err_log; _} problem =
    let loc     = problem.loc                                                             in
    let issue   = problem.issue                                                           in 
    let message = F.asprintf "Read Copy Update problem: %a" problemSetPrint problem       in
    Reporting.log_issue proc_desc err_log ~loc:loc ReadCopyUpdateViolation
       issue message

let printProblems interprocedural post = ProblemSet.iter (report interprocedural) post.problems 


(** Operands *)
(*********************************************************** *)

let leq ~lhs ~rhs = ProblemSet.subset lhs.problems rhs.problems && LockSet.subset lhs.post rhs.post  

(** Unite sets *)
let join astateA astateB = let newProblems = ProblemSet.union astateA.problems astateB.problems in
                           let lockList    = set2list astateA.post                              in 
                           let newAstate   = joinElements lockList astateB                      in
                           {problems = newProblems; post = newAstate.post}
                
(** We join and join *)
let widen ~prev ~next ~num_iters:_ = join prev next

(** Print lock name and score *)
let pp fmt astate = LockSet.iter (lockSetPrint fmt) astate.post

let hasViolation summary = not (ProblemSet.is_empty summary.problems)


(** Summary *)
(************************************************************************************* *)        

(** TBD *)
module Summary = struct
                        
        let updateSummary astate summary = join astate summary     (** Updates a current summary with abstract state *)
end 


(** Create summary -> checks if any problems have been resolved in the ProblemSet *)

(** Add scores of locks + maybe join problems, maybe problems will need to be joined manualy as well *)
let applySummary astate summary = if ProblemSet.is_empty summary.problems && LockSet.is_empty summary.post then astate
                                  else 
                                      let newProblemSet = ProblemSet.union astate.problems summary.problems in
                                      let summaryList   = set2list summary.post                             in
                                      let newAstate     = addElements summaryList astate                    in 
                                      {problems = newProblemSet; post = newAstate.post}

                                              

(** Problem -> the order of functions in the analysis 
    Problem with Synchronize needs to be saved, because if somebody unlocked before locking inside the function is not a problem, although it is not 
    recommended either*)                              

type summary = t
