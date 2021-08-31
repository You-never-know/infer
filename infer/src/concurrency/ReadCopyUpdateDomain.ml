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
{             lockName     : string;         (** name of the lock *)
      mutable lockScoreMin : int;            (** score of the lock - start of the interval *)
      mutable lockScoreMax : int;            (** score of the lock - end of the interval *)
              accessPath   : AccessPath.base;(** access path of the lock *)
              loc          : Location.t      (** line of code where the lock was locked *)
}

module LockSet = Set.Make(struct
        type t = lockInfo
        let compare a b = String.compare a.lockName b.lockName 
end)

let lockSetPrint fmt lock = F.fprintf fmt "Lock %a on %a has score: <%a,%a>" String.pp lock.lockName Location.pp lock.loc 
                                                                             Int.pp lock.lockScoreMin Int.pp lock.lockScoreMax


type problem = 
{
            islockProblem   : bool;       (** Determines if a problem is related to some lock *)
            lockNeeded      : bool;       (** In a case of rcu_dereference, when no lock has been used yet, 
                                              or multiple locks are used and not one is locked, if true -> false this probme is removed *)
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

(** Combine locks -> take the lower min score and the higher max score *)
let combineLocks (lockA : lockInfo) (lockB : lockInfo) = let newLockScoreMax = max lockA.lockScoreMax lockB.lockScoreMax in
                                                         let newLockScoreMin = min lockA.lockScoreMin lockB.lockScoreMin in 
                                                         {
                                                          lockName = lockB.lockName; 
                                                          lockScoreMin = newLockScoreMin;
                                                          lockScoreMax = newLockScoreMax; 
                                                          loc = lockB.loc; 
                                                          accessPath = lockB.accessPath
                                                         } 

(** Add lock scores, and take the newer accessPath as well as the newer loc *)
let addLockScores (lockA : lockInfo) (lockB : lockInfo) = let newLockScoreMax = lockA.lockScoreMax + lockB.lockScoreMax   in
                                                          let newLockScoreMin = lockA.lockScoreMin + lockB.lockScoreMin   in
                                                          {
                                                           lockName = lockB.lockName; 
                                                           lockScoreMin = newLockScoreMin;
                                                           lockScoreMax = newLockScoreMax; 
                                                           loc = lockB.loc; 
                                                           accessPath = lockB.accessPath
                                                          } 


(** check if the lock is a part of abstract state, if it is combine it with the one from summary, if not add it to the astate *)
let joinLockWithAstate (lock : lockInfo) (astate : t) = if lockSetMember lock astate then 
                                                             let foundLock    = LockSet.find lock astate.post        in
                                                             let combinedLock = combineLocks foundLock lock          in 
                                                             let removedSet   = LockSet.remove lock astate.post      in
                                                             let newPost      = LockSet.add combinedLock removedSet  in
                                                             {problems = astate.problems; post = newPost}
                                                        else 
                                                             let newPost      = LockSet.add lock astate.post         in 
                                                             {problems = astate.problems; post = newPost} 
                        

(** check if the lock is a part of abstract state, if it is combine it with the one from summary, if not add it to the astate *)
let joinLockWithSummary (lock : lockInfo) (astate : t) = if lockSetMember lock astate then 
                                                             let foundLock    = LockSet.find lock astate.post        in
                                                             let combinedLock = addLockScores foundLock lock         in 
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

let createLock lockName lockScoreMin lockScoreMax accessPath loc = {
                                                                    lockName = lockName; 
                                                                    lockScoreMin = lockScoreMin; 
                                                                    lockScoreMax = lockScoreMax; 
                                                                    accessPath = accessPath; 
                                                                    loc = loc
                                                                   } 

let addLock lock astate = let newElement = lock                               in 
                          let newPost    = LockSet.add newElement astate.post in 
                          {problems = astate.problems; post = newPost}
                                                  

let unlock2lock lockName = if      String.equal lockName "urcu_memb_read_unlock"          then "urcu_memb_read_lock"
                           else if String.equal lockName "urcu_mb_read_unlock"            then "urcu_mb_read_lock"
                           else if String.equal lockName "urcu_bp_read_unlock"            then "urcu_bp_read_lock"
                           else if String.equal lockName "urcu_signal_read_unlock"        then "urcu_signal_read_lock"
                           else if String.equal lockName "urcu_qsbr_read_unlock"          then "urcu_qsbr_read_lock"
                           else if String.equal lockName "rcu_read_unlock"                then "rcu_read_lock"
                           else if String.equal lockName "rcu_read_unlock_bh"             then "rcu_read_lock_bh"
                           else if String.equal lockName "local_bh_enable"                then "local_bh_disable"
                           else if String.equal lockName "rcu_read_unlock_sched"          then "rcu_read_lock_sched"
                           else if String.equal lockName "rcu_read_unlock_sched_notrace"  then "rcu_read_unlock_sched_notrace"
                           else if String.equal lockName "preempt_enable"                 then "preempt_disable"
                           else if String.equal lockName "local_irq_restore"              then "local_irq_save"
                           else if String.equal lockName "srcu_read_unlock"               then "srcu_read_lock"
                           else if String.equal lockName "srcu_read_unlock_notrace"       then "srcu_read_lock_notrace"
                           else    lockName

let increaseLockScore lock astate = let currentLock = LockSet.find lock astate.post                                                                      in
                                    let newLock     = createLock currentLock.lockName (currentLock.lockScoreMin + 1) (currentLock.lockScoreMax + 1) 
                                                                 currentLock.accessPath currentLock.loc                                                  in
                                    let removedSet  = LockSet.remove currentLock astate.post                                                             in 
                                    let newPost     = LockSet.add newLock removedSet                                                                     in 
                                    {problems = astate.problems; post = newPost} 

let decreaseLockScore lock astate = let currentLock = LockSet.find lock astate.post                                                                      in
                                    let newLock     = createLock currentLock.lockName (currentLock.lockScoreMin - 1) (currentLock.lockScoreMax - 1)
                                                                 currentLock.accessPath currentLock.loc                                                  in
                                    let removedSet  = LockSet.remove currentLock astate.post                                                             in 
                                    let newPost     = LockSet.add newLock removedSet                                                                     in 
                                    {problems = astate.problems; post = newPost}      


(** ProblemSet functions *) 



(** Better to check for the top of the interval, because it is better to show a false positive than miss an error *)
let checkIfLocked lock = if lock.lockScoreMax > 0 then true
                         else false

let addProblem problem astate = let newProblemSet = ProblemSet.add problem astate.problems    in
                                {problems = newProblemSet; post = astate.post} 

let addDeprecatedWarning ~problemLock ~procName ~loc astate = 
                let newProblem    = {islockProblem = false;
                                     lockNeeded = false; 
                                     problemLock = problemLock; 
                                     procName = procName; 
                                     loc = loc;
                                     problemName = "Deprecated function call used"; 
                                     issue = IssueType.rcu_deprecated_problem}                in                                           
                addProblem newProblem astate

let createProblem ~isLockProblem ~lockNeeded ~problemLock ~functionName ~loc problemDescription issue = {
                                                                                                           islockProblem = isLockProblem;
                                                                                                           lockNeeded = lockNeeded;
                                                                                                           problemLock = problemLock;
                                                                                                           procName = functionName;
                                                                                                           problemName = problemDescription;
                                                                                                           loc = loc;
                                                                                                           issue = issue
                                                                                                          } 


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


(** RCU functions *)
let getFlavourLocks functionName accesPath loc = if      String.equal functionName "urcu_memb_synchronize_rcu"                      then 
                                                            let lock = createLock "urcu_memb_read_lock" 0 0 accesPath loc           in 
                                                            [lock]
                                                 else if String.equal functionName "urcu_mb_synchronize_rcu"                        then 
                                                            let lock = createLock "urcu_mb_read_lock" 0 0 accesPath loc             in 
                                                            [lock] 
                                                 else if String.equal functionName "urcu_bp_synchronize_rcu"                        then 
                                                            let lock = createLock "urcu_bp_read_lock" 0 0 accesPath loc             in 
                                                            [lock]  
                                                 else if String.equal functionName "urcu_signal_synchronize_rcu"                    then
                                                            let lock = createLock "urcu_signal_read_lock" 0 0 accesPath loc         in 
                                                            [lock]
                                                 else if String.equal functionName "urcu_qsbr_synchronize_rcu"                      then
                                                            let lock = createLock "urcu_qsbr_read_lock" 0 0 accesPath loc           in 
                                                            [lock]
                                                 else if String.equal functionName "synchronize_rcu"                                ||
                                                         String.equal functionName "synchronize_net"                                ||
                                                         String.equal functionName "synchronize_rcu_expedited"                      then    
                                                            let lock1 = createLock "rcu_read_lock" 0 0 accesPath loc                in 
                                                            let lock2 = createLock "rcu_read_lock_bh" 0 0 accesPath loc             in
                                                            let lock3 = createLock "local_bh_disable" 0 0 accesPath loc             in 
                                                            let lock4 = createLock "rcu_read_lock_sched" 0 0 accesPath loc          in 
                                                            let lock5 = createLock "rcu_read_lock_sched_notrace" 0 0 accesPath loc  in 
                                                            let lock6 = createLock "preempt_disable" 0 0 accesPath loc              in
                                                            let lock7 = createLock "local_irq_save" 0 0 accesPath loc               in
                                                            [lock1; lock2; lock3; lock4; lock5; lock6; lock7]
                                                 else if String.equal functionName "synchronize_srcu"                               ||
                                                         String.equal functionName "synchronize_srcu_expedited"                     ||
                                                         String.equal functionName "synchronize_rcu_tasks"                          then  
                                                            let lock1 = createLock "srcu_read_lock" 0 0 accesPath loc               in 
                                                            let lock2 = createLock "srcu_read_lock_notrace" 0 0 accesPath loc       in
                                                            [lock1; lock2]
                                                 else  
                                                            let lock = createLock "no_lock" 0 0 accesPath loc                       in 
                                                            [lock]

let isSynchronize functionName = if      String.equal functionName "urcu_memb_synchronize_rcu"   then true 
                                 else if String.equal functionName "urcu_mb_synchronize_rcu"     then true 
                                 else if String.equal functionName "urcu_bp_synchronize_rcu"     then true 
                                 else if String.equal functionName "urcu_signal_synchronize_rcu" then true
                                 else if String.equal functionName "urcu_qsbr_synchronize_rcu"   then true
                                 else if String.equal functionName "synchronize_rcu"             then true
                                 else if String.equal functionName "synchronize_net"             then true
                                 else if String.equal functionName "synchronize_rcu_expedited"   then true
                                 else if String.equal functionName "synchronize_srcu"            then true
                                 else if String.equal functionName "synchronize_srcu_expedited"  then true
                                 else if String.equal functionName "synchronize_rcu_tasks"       then true
                                 else false 
                                 
let isDepracated functionName = if      String.equal functionName "synchronize_rcu_bh"            then true 
                                else if String.equal functionName "synchronize_rcu_bh_expedited"  then true 
                                else if String.equal functionName "call_rcu_bh"                   then true 
                                else if String.equal functionName "rcu_barrier_bh"                then true
                                else if String.equal functionName "synchronize_sched"             then true
                                else if String.equal functionName "synchronize_sched_expedited"   then true
                                else if String.equal functionName "call_rcu_sched"                then true
                                else if String.equal functionName "rcu_barrier_sched"             then true
                                else if String.equal functionName "get_state_synchronize_sched"   then true
                                else if String.equal functionName "cond_synchronize_sched"        then true
                                else if String.equal functionName "synchronize_rcu_mult"          then true
                                else if String.equal functionName "rcu_access_index"              then true
                                else if String.equal functionName "rcu_dereference_index_check"   then true
                                else if String.equal functionName "rcu_lockdep_assert"            then true
                                else if String.equal functionName "hlist_add_after_rcu"           then true
                                else false 


let rec findLock list post = if not (List.length list <> 0)      then false 
                             else 
                                let firstElement = List.hd list                       in
                                if LockSet.mem firstElement post then true 
                                else 
                                    let newList  = List.tl list                       in 
                                    findLock newList post   


let rec lockListMem element list = if not (List.length list <> 0) then false
                                   else 
                                       let firstElement = List.hd list                                          in
                                       if String.equal element.lockName firstElement.lockName then true
                                       else 
                                           let newList  = List.tl list                                          in 
                                           lockListMem element newList 


let rec synchronizeProblem lockList postList = if not (List.length postList <> 0) then None  
                                               else 
                                                   let firstElement = List.hd postList                                                       in
                                                   if lockListMem firstElement lockList then 
                                                        if checkIfLocked firstElement then Some firstElement 
                                                        else 
                                                        let newList  = List.tl postList                                                       in 
                                                       synchronizeProblem lockList newList 
                                                   else 
                                                       let newList  = List.tl postList                                                       in 
                                                       synchronizeProblem lockList newList  
                                                


let findProblem functionName accessPath loc astate = let lockList = getFlavourLocks functionName accessPath loc             in
                                                     (** Critical section for the synchronize_rcu found *)
                                                     if findLock lockList astate.post then 
                                                         let postList = set2list astate.post                                in
                                                         match synchronizeProblem lockList postList with
                                                         | None -> None 
                                                         | Some (lock) -> 
                                                                let error = createProblem ~isLockProblem:false ~lockNeeded:false ~problemLock:lock
                                                                            ~functionName:functionName ~loc:loc 
                                                                            "Deadlock -> synchronization inside 
                                                                             a critical section"
                                                                            IssueType.rcu_synchronization_problem           in 
                                                                Some error     
                                                     (** No critical section for the synchronize_rcu flavour found *)
                                                     else 
                                                         let randomLock = List.hd lockList                                  in
                                                         let warning = createProblem ~isLockProblem:false ~lockNeeded:false 
                                                                       ~problemLock:randomLock ~functionName:functionName 
                                                                       ~loc:loc 
                                                                       "RCU flavour potencial mismatch - 
                                                                        RCU flavour of synchronization primitive
                                                                        does not match any detected critical section"
                                                                       IssueType.rcu_flavour_problem                        in
                                                         Some warning                  
                                                     
                                                   


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
