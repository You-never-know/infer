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
include AbstractDomain.S

type lockInfo =   
{             lockName    : string;     (** name of the lock *)
      mutable lockScore   : int;            (** score of the lock ("2" locked 2 times|"-2" twice unlocked *)
              accessPath  : AccessPath.base;(** access path of the lock *)
              loc         : Location.t      (** line of code where the lock was locked *)
}

type problem = 
{
            lockNeeded      : bool;       (** In a case of rcu_dereference, when no lock has been used yet, or multiple locks are used and not one is locked, if true -> false this probme is removed *)
            problemLock     : lockInfo;   (** Info about a problematic lock, in a case of lock score going to 0, problem is removed *)
            procName        : string;     (** Name of the function where a problem was detected *)
            loc             : Location.t; (** Line of code, where the problem was detected *)
            problemName     : string;     (** Info about the problem *)
            issue           : IssueType.t (** Type of the issue *)
}


module Summary : sig

      val updateSummary        : t -> t -> t   (** Updates a current summary with abstract state *)

end

val createLock                 : string -> int -> AccessPath.base -> Location.t -> lockInfo

val addLock                    : lockInfo -> t -> t

val lockSetMember              : lockInfo -> t -> bool

val unlock2lock                : string -> string 

val increaseLockScore          : lockInfo  -> t -> t

val decreaseLockScore          : lockInfo  -> t -> t

val applySummary               : t -> t -> t

val findProblemLock            : t -> lockInfo option

val printProblems              : t InterproceduralAnalysis.t -> t -> unit

val addSynchronizationProblem  : problemLock:lockInfo -> procName:string -> loc:Location.t -> t -> t 

val isSynchronize              : string -> bool

val isDepracated               : string -> bool 

val hasViolation               : t -> bool 

val initial                    : t

type summary = t
