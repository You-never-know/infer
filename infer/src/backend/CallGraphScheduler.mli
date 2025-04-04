(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd

val bottom_up : CallGraph.t -> (TaskSchedulerTypes.target, 'a, _) TaskGenerator.t
