(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

include AbstractDomain.S

val rcu_lock : t -> t

val rcu_unlock : t -> t

val has_violation : t -> bool

val initial : t

type summary = t
