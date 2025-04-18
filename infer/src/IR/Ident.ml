(*
 * Copyright (c) 2009-2013, Monoidics ltd.
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

(** Module for Names and Identifiers *)

open! IStd
module Hashtbl = Stdlib.Hashtbl
module F = Format

module Name = struct
  type t = Primed | Normal | Footprint | Spec | FromString of string
  [@@deriving compare, yojson_of, equal, sexp, hash, normalize]

  let primed = "t"

  let normal = "n"

  let footprint = "f"

  let spec = "val"

  let from_string s = FromString s

  let to_string = function
    | Primed ->
        primed
    | Normal ->
        normal
    | Footprint ->
        footprint
    | Spec ->
        spec
    | FromString s ->
        s
end

type name = Name.t [@@deriving compare, equal, sexp, hash, normalize]

type kind =
  | KNone
      (** special kind of "null ident" (basically, a more compact way of implementing an ident
          option). useful for situations when an instruction requires an id, but no one should read
          the result. *)
  | KFootprint
  | KNormal
  | KPrimed
[@@deriving compare, yojson_of, equal, sexp, hash, normalize]

let kfootprint = KFootprint

let knormal = KNormal

let kprimed = KPrimed

let knone = KNone

type t = {kind: kind; name: Name.t; stamp: int}
[@@deriving compare, yojson_of, sexp, hash, equal, normalize]

(* most unlikely first *)
let equal i1 i2 =
  Int.equal i1.stamp i2.stamp && equal_kind i1.kind i2.kind && equal_name i1.name i2.name


(** {2 Set for identifiers} *)
module Set = Stdlib.Set.Make (struct
  type nonrec t = t [@@deriving compare]
end)

module Map = Stdlib.Map.Make (struct
  type nonrec t = t [@@deriving compare]
end)

(** {2 Conversion between Names and Strings} *)
module NameHash = Hashtbl.Make (struct
  type t = name [@@deriving equal, hash]
end)

(** Convert a string to a name *)
let string_to_name = Name.from_string

(** Convert a name to a string. *)
let name_to_string = Name.to_string

(** {2 Functions and Hash Tables for Managing Stamps} *)

(** Get the stamp of the identifier *)
let get_stamp i = i.stamp

module NameGenerator = struct
  type t = int NameHash.t

  let create () : t = NameHash.create 17

  (** Map from names to stamps. *)
  let name_map = DLS.new_key create

  let get_current () = DLS.get name_map

  let set_current map = DLS.set name_map map

  (** Reset the name generator *)
  let reset () = DLS.set name_map (create ())

  (** Create a fresh identifier with the given kind and name. *)
  let create_fresh_ident kind name =
    let stamp =
      try
        let stamp = NameHash.find (DLS.get name_map) name in
        NameHash.replace (DLS.get name_map) name (stamp + 1) ;
        stamp + 1
      with Stdlib.Not_found ->
        NameHash.add (DLS.get name_map) name 0 ;
        0
    in
    {kind; name; stamp}


  (** Make sure that fresh ids after whis one will be with different stamps *)
  let update_name_hash name stamp =
    try
      let curr_stamp = NameHash.find (DLS.get name_map) name in
      let new_stamp = max curr_stamp stamp in
      NameHash.replace (DLS.get name_map) name new_stamp
    with Stdlib.Not_found -> NameHash.add (DLS.get name_map) name stamp
end

(** Name used for the return variable *)
let name_return = Mangled.from_string "return"

(** Return the standard name for the given kind *)
let standard_name kind =
  if equal_kind kind KNormal || equal_kind kind KNone then Name.Normal
  else if equal_kind kind KFootprint then Name.Footprint
  else Name.Primed


(** Every identifier with a given stamp should ultimately be created using this function *)
let create_with_stamp kind name stamp =
  NameGenerator.update_name_hash name stamp ;
  {kind; name; stamp}


(** Create an identifier with default name for the given kind *)
let create kind stamp = create_with_stamp kind (standard_name kind) stamp

(** Generate a normal identifier with the given name and stamp *)
let create_normal name stamp = create_with_stamp KNormal name stamp

(** Create a fresh identifier with default name for the given kind. *)
let create_fresh kind = NameGenerator.create_fresh_ident kind (standard_name kind)

let create_none () = create_fresh KNone

(** {2 Functions for Identifiers} *)

let has_kind id kind = equal_kind id.kind kind

let is_normal (id : t) = has_kind id KNormal || has_kind id KNone

let is_footprint (id : t) = has_kind id KFootprint

let is_none (id : t) = has_kind id KNone

(** Update the name generator so that the given id's are not generated again *)
let update_name_generator ids =
  let upd id = ignore (create_with_stamp id.kind id.name id.stamp) in
  List.iter ~f:upd ids


(** {2 Pretty Printing} *)

(** Pretty print an identifier. *)
let pp f id =
  if has_kind id KNone then F.pp_print_char f '_'
  else
    let base_name = name_to_string id.name in
    let prefix = if has_kind id KFootprint then "@" else if has_kind id KNormal then "" else "_" in
    F.fprintf f "%s%s$%d" prefix base_name id.stamp


(** Convert an identifier to a string. *)
let to_string id = F.asprintf "%a" pp id

(** Pretty print a name. *)
let pp_name f name = F.pp_print_string f (name_to_string name)

module HashQueue = Hash_queue.Make (struct
  type nonrec t = t [@@deriving compare, hash]

  let sexp_of_t id = Sexp.of_string (to_string id)
end)

let hashqueue_of_sequence ?init s =
  let q = match init with None -> HashQueue.create () | Some q0 -> q0 in
  Sequence.iter s ~f:(fun id ->
      let (_ : [`Key_already_present | `Ok]) = HashQueue.enqueue_back q id () in
      () ) ;
  q
