(* Author: Dominik Harmim <iharmim@fit.vut.cz> *)

open! IStd
module F = Format

(** Atomicity violations analysis utilities interface. *)

(* ************************************ Pretty Printable **************************************** *)

val pp_icollection : pp_item:(F.formatter -> int * 'a -> unit) -> F.formatter -> 'a list -> unit
(** A pretty-printable function for printing a collection of items with their indices. *)

val pp_collection : pp_item:(F.formatter -> 'a -> unit) -> F.formatter -> 'a list -> unit
(** A pretty-printable function for printing a collection of items. *)

(** A pretty-printable set. *)
module type PPSet = sig
  include PrettyPrintable.PPSet
end

(** A module for making the pretty-printable set. *)
module MakePPSet (Ord : PrettyPrintable.PrintableOrderedType) : PPSet with type elt = Ord.t

(** A pretty-printable map. *)
module type PPMap = sig
  include PrettyPrintable.PPMap
end

(** A module for making the pretty-printable map. *)
module MakePPMap (Ord : PrettyPrintable.PrintableOrderedType) : PPMap with type key = Ord.t

(* ************************************ Modules ************************************************* *)

(** A module that represents a lock's access path with the number of times the lock has been
    acquired. *)
module Lock : sig
  include PrettyPrintable.PrintableEquatableOrderedType

  val lock : t -> t
  (** Increases the number of times the lock has been acquired. *)

  val unlock : t -> t
  (** Decreases the number of times the lock has been acquired. *)

  val is_locked : t -> bool
  (** Checks whether the lock is locked at least once. *)

  val create : AccessPath.t -> t
  (** Constructs the module from an access path of a lock. *)

  val path : t -> AccessPath.t
  (** Returns an access path of the lock. *)
end

(** A module that represents associations between lock guards and locks. *)
module Guards : sig
  include PrettyPrintable.PrintableEquatableOrderedType

  val empty : t
  (** Creates an empty module. *)

  val add : AccessPath.t -> AccessPath.t list -> t -> t
  (** Adds a new association between a lock guard and locks. *)

  val remove : AccessPath.t -> t -> t
  (** Removes an existing association between a lock guard and locks. *)

  val reveal_locks : t -> AccessPath.t list -> AccessPath.t list
  (** Changes a list of access paths as follows: i) If the access path in the list belongs to a
      lock, the access path remains unchanged. ii) If the access path in the list belongs to a lock
      guard, the access path is removed from the list, and all the access paths of this guard's
      locks are appended. *)
end

(* ************************************ Constants *********************************************** *)

val atomic_sets_file : string
(** A file for storing atomic sets. *)

val file_comment_char : char
(** A character used for commenting in files. *)

(* ************************************ Functions *********************************************** *)

val assert_user : bool -> ('a, F.formatter, unit) format -> 'a
(** An assertion with a user message. *)

val is_line_empty : string -> bool
(** Checks whether a line in a file is empty. *)

val f_is_ignored : ?actuals:HilExp.t list option -> Procname.t -> bool
(** Checks whether a given function is ignored. *)

val get_exps_paths : HilExp.t list -> AccessPath.t list
(** Returns access paths of given expressions. *)

val get_exp_path : HilExp.t -> AccessPath.t
(** Returns an access path of a given expression. *)

val proc_name_to_access_path : Procname.t -> AccessPath.t
(** Converts a procedure name to an artificial access path. *)

val is_local_call_ignored : Procdesc.t -> actuals:HilExp.t list -> bool
(** Checks whether a function call in a given function has local objects in its parameters and
    should thus be ignored. *)

val file_summaries : 'a InterproceduralAnalysis.file_t -> (Procname.t * 'a) list
(** Returns a list of analysed functions and their summaries from a given file. *)
