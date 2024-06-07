(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
open! LineageShape.StdModules

module TaintConfig : sig
  type t

  val parse :
       lineage_source:string option
    -> lineage_sink:string option
    -> lineage_sanitizers:string list
    -> lineage_limit:int option
    -> t option
end

val report : TaintConfig.t -> unit

include sig
  (** Used for tests*)

  [@@@warning "-unused-module"]

  [@@@warning "-unused-type-declaration"]

  [@@@warning "-unused-value-declaration"]

  module Private : sig
    module Todo : sig
      type node

      val pp_node : node Fmt.t

      type t = {procname: Procname.t; node: node}
    end

    module TaintConfig : sig
      val parse_node : string -> Todo.t option
    end
  end
end
