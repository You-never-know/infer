(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format

type t = int

let leq ~lhs ~rhs = lhs <= rhs 

let join a b = max a b

let widen ~prev ~next ~num_iters:_ = join prev next

let pp fmt astate = F.fprintf fmt "%d" astate

let rcu_lock locks_locked = locks_locked + 1

let rcu_unlock locks_locked = locks_locked - 1 

let has_violation locks_locked = locks_locked <> 0  

let initial = 0

type summary = t
