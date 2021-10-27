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
{             lockName     : string;         (** name of the lock *)
      mutable lockScoreMin : int;            (** score of the lock - start of the interval *)
      mutable lockScoreMax : int;            (** score of the lock - end of the interval *)
              accessPath   : AccessPath.base;(** access path of the lock *)
              loc          : Location.t      (** line of code where the lock was locked *)
}

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


module Summary : sig
      val updateSummary        : t -> t -> t   (** Updates a current summary with abstract state *)
end

module FunctionSet : sig
      type t 
      val compare              : t -> t -> int
end

module ProblemSet : sig
        type t 
        val compare              : t -> t -> int
end

val functionCallsInit          : FunctionSet.t

val getFunctionsCalled         : 'a list -> ('a -> (Procdesc.t * t) option) -> FunctionSet.t

val diffSets                   : ProblemSet.t -> ProblemSet.t -> ProblemSet.t 

val addFunctionCall            : Procname.t -> t -> t

val removeFunctionCall         : Procname.t -> t -> t

val createLock                 : string -> int -> int -> AccessPath.base -> Location.t -> lockInfo

val addLock                    : lockInfo -> t -> t

val lockSetMember              : lockInfo -> t -> bool

val unlock2lock                : string -> string 

val increaseLockScore          : lockInfo  -> t -> t

val decreaseLockScore          : lockInfo  -> t -> t

val applySummary               : t -> t -> t

val findProblem                : string -> AccessPath.base -> Location.t -> t -> problem option

val checkIfLocked              : lockInfo -> bool 

val printProblems              : t InterproceduralAnalysis.t -> t -> unit

val addProblem                 : problem -> t -> t 

val addDeprecatedWarning       : problemLock:lockInfo -> procName:string -> loc:Location.t -> t -> t 

val isSynchronize              : string -> bool 

val isDepracated               : string -> bool 

val hasViolation               : t -> bool 

val initial                    : t

type summary = t
