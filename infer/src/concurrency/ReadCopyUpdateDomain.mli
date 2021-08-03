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
include AbstractDomain.S

type lockInfo = 
{             lockName    : Procname.t;  (** name of the lock *)
      mutable lockScore   : int;         (** score of the lock ("2" locked 2 times|"-2" twice unlocked *)
              accessPath  : AccessPath.t;(** access path of the lock *)
              loc         : Location.t   (** line of code where the lock was locked *)
}

type problem = 
{
            procName        : Procname.t; (** Name of the function where a problem was detected *)
            loc             : Location.t; (** Line of code, where the problem was detected *)
            problemName     : string;     (** Info about the problem *)
            issue           : IssueType.t (** Type of the issue *)
}


val createLock                 : Procname.t -> int -> AccessPath.t -> Location.t -> lockInfo

val addLock                    : lockInfo -> t -> t

val member                     : lockInfo -> t -> bool

val initial                    : t

type summary = t
