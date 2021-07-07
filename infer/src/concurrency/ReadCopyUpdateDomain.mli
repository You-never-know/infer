(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
include AbstractDomain.S
module Hash = Caml.Hashtbl

val check_if_lock_in_current_state   : string -> t -> bool

val initial                          : t

type summary = t
