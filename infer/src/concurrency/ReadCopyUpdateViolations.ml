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
module F = Format
module L = Logging

module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = ReadCopyUpdateDomain 

  type analysis_data = ReadCopyUpdateDomain.t InterproceduralAnalysis.t


  (** Take an abstract state and instruction, produce a new abstract state *)
  let exec_instr (astate : ReadCopyUpdateDomain.t)
      {InterproceduralAnalysis.proc_desc= _; tenv= _; analyze_dependency; _} _
      (instr : HilInstr.t) =
    match instr with
    | Call (( path : AccessPath.base),(Direct (calleeProc : Procname.t) : HilInstr.call), 
           (actuals : HilExp.t list), (_ : CallFlags.t), (loc : Location.t) ) -> (
        (* function call [return_opt] := invoke [callee_procname]([actuals]) *)
        (* L.progress "Computed post %a \n" Location.pp loc; *)
        match 
          ConcurrencyModels.get_lock_effect calleeProc actuals
        with 
          (* RCU lock - multiple flavours *)
          | RCULock (_locks: HilExp.t list) -> 
                (* Domain.rcu_lock astate *)
                let lock      = Domain.createLock (Procname.to_string calleeProc) 1 path loc                      in
                if Domain.lockSetMember lock astate then Domain.increaseLockScore lock astate
                else Domain.addLock lock astate                                                                    
          (* RCU unlock - multiple flavours *)
          | RCUUnlock (_locks: HilExp.t list) -> 
                let lockName  = Domain.unlock2lock (Procname.to_string calleeProc)                                in 
                let lock      = Domain.createLock lockName (-1) path loc                                          in 
                if Domain.lockSetMember lock astate then Domain.decreaseLockScore lock astate     
                else Domain.addLock lock astate                                                
          | _ -> (** find functions (rcu_dereference and synchronize_rcu and check if there is not a problem) *)
            (** Other function call *)
                let functionName = Procname.to_string calleeProc in
                if String.equal "rcu_dereference" functionName then astate 
                else if String.equal "synchronize_rcu" functionName then astate 
                (** Any other function call, we may have a summary for this funtion alredy *)
                else  
                    match 
                      analyze_dependency calleeProc 
                    with
                      | Some (_callee_proc_desc, callee_summary) -> Domain.applySummary astate callee_summary
                      | None -> astate
                ) 
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


(**
Plan

3. rework the report function to be called just at the end, where it will report all the issues 


Maybe: create summary (Actual state + problems found)

!!!!!!!!!!!!!!!!!!!! Idea !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
Summary holds the problems that cannot be fixed, at the end a final summary is created with all the problems from abstract state, and this summary is forwarded into a print function that prints all the problems 
*)

(** Report an error when we have acquired more resources than we have released *)
let report_if_violated {InterproceduralAnalysis.proc_desc; err_log; _} post =
  let result = true in
  if result then
    let last_loc = Procdesc.Node.get_loc (Procdesc.get_exit_node proc_desc)                                  in
    let message = F.asprintf "RCU locks locked at the end of the procedure: %a" ReadCopyUpdateDomain.pp post in
    Reporting.log_issue proc_desc err_log ~loc:last_loc ReadCopyUpdateViolation
      IssueType.read_copy_update_violation message

(** Main function into the checker--registered in RegisterCheckers *)
let checker (analysisData : ReadCopyUpdateDomain.summary InterproceduralAnalysis.t) :
    ReadCopyUpdateDomain.summary option =
      let init = ReadCopyUpdateDomain.initial in 
      match Analyzer.compute_post analysisData ~initial:init analysisData.proc_desc with 
      (** An abstract state has been created, make summary *)
      | Some (procedureAstate : ReadCopyUpdateDomain.t) ->
         report_if_violated analysisData procedureAstate ;Some (ReadCopyUpdateDomain.Summary.updateSummary procedureAstate ReadCopyUpdateDomain.initial )
      | None -> 
        L.die InternalError "The detection of read copy update violations failed to compute a post for '%a'." Procname.pp (Procdesc.get_proc_name analysisData.proc_desc)
          
      
      
      
      (* let procedureAstate = Analyzer.compute_post analysisData ~initial:ReadCopyUpdateDomain.initial analysisData.proc_desc in
      Option.iter result ~f:(fun post -> report_if_violated analysisData post) ;
      result *)
