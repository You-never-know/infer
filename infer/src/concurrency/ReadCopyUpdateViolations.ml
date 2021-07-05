(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = ReadCopyUpdateDomain

  type analysis_data = ReadCopyUpdateDomain.t InterproceduralAnalysis.t


  (** Take an abstract state and instruction, produce a new abstract state *)
  let exec_instr (astate : ReadCopyUpdateDomain.t)
      {InterproceduralAnalysis.proc_desc= _; tenv= _; analyze_dependency= _; _} _
      (instr : HilInstr.t) =
    match instr with
    | Call (_return_opt, Direct callee_procname, actuals, _, _loc) -> (
        (* function call [return_opt] := invoke [callee_procname]([actuals]) *)
        (* L.progress "Computed post %a \n" Procname.pp callee_procname; *)
        match 
          ConcurrencyModels.get_lock_effect callee_procname actuals
        with 
        (* RCU lock *)
        | RCULock (_locks: HilExp.t list) -> 
            (* Domain.rcu_lock astate *)
            Domain.rcu_lock astate
        (* RCU unlock *)
        | RCUUnlock (_locks: HilExp.t list) -> 
            Domain.rcu_unlock astate
        | _ ->
          astate )
    | Assign (_lhs_access_path, _rhs_exp, _loc) ->
        (* an assignment [lhs_access_path] := [rhs_exp] *)
        astate
    | Assume (_assume_exp, _, _, _loc) ->
        (* a conditional assume([assume_exp]). blocks if [assume_exp] evaluates to false *)
        astate
    | Call (_, Indirect _, _, _, _) ->
        (* This should never happen in Java. Fail if it does. *)
        L.(die InternalError) "Unexpected indirect call %a" HilInstr.pp instr
    | Metadata _ ->
        astate


  let pp_session_name _node fmt = F.pp_print_string fmt "Read Copy Update Violation"
end


module CFG = ProcCfg.Normal
(* Create an intraprocedural abstract interpreter from the transfer functions defined earlier*)
module Analyzer = LowerHil.MakeAbstractInterpreter (TransferFunctions (CFG))

(** Report an error when we have acquired more resources than we have released *)
let report_if_violated {InterproceduralAnalysis.proc_desc; err_log; _} post =
  let result = ReadCopyUpdateDomain.has_violation post in
  if result then
    let last_loc = Procdesc.Node.get_loc (Procdesc.get_exit_node proc_desc) in
    let message = F.asprintf "RCU locks locked at the end of the procedure: %a" ReadCopyUpdateDomain.pp post in
    Reporting.log_issue proc_desc err_log ~loc:last_loc ReadCopyUpdateViolation
      IssueType.read_copy_update_violation message


(** Main function into the checker--registered in RegisterCheckers *)
let checker ({InterproceduralAnalysis.proc_desc} as analysis_data) =
  let result = Analyzer.compute_post analysis_data ~initial:ReadCopyUpdateDomain.initial proc_desc in
  Option.iter result ~f:(fun post -> report_if_violated analysis_data post) ;
  result
