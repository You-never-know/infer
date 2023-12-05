(* Author: Dominik Harmim <iharmim@fit.vut.cz> *)

open! IStd
open AtomicityUtils
module Domain = AtomicityViolationsDomain
module F = Format
module L = Logging

(** Detection of atomicity violations implementation. *)

(** A transfer function for abstract states of an analysed function. *)
module TransferFunctions (CFG : ProcCfg.S) = struct
  module CFG = CFG
  module Domain = Domain

  type analysis_data = Domain.Summary.t InterproceduralAnalysis.t

  let exec_instr (astate : Domain.t) (analysis_data : analysis_data) (_ : CFG.Node.t)
      (_ : ProcCfg.InstrNode.instr_index) : HilInstr.t -> Domain.t = function
    | Call
        ( (_ : AccessPath.base)
        , (Direct (callee_pname : Procname.t) : HilInstr.call)
        , (actuals : HilExp.t list)
        , (_ : CallFlags.t)
        , (_ : Location.t) )
      when f_is_ignored callee_pname ~actuals:(Some actuals) ->
        astate
    (* Update the abstract state on function calls. *)
    | Call
        ( (_ : AccessPath.base)
        , (Direct (callee_pname : Procname.t) : HilInstr.call)
        , (actuals : HilExp.t list)
        , (_ : CallFlags.t)
        , (loc : Location.t) ) -> (
      match
        ConcurrencyModels.get_lock_effect ~tenv:(Some analysis_data.tenv) callee_pname actuals
      with
      (* lock *)
      | Lock (locks : HilExp.t list) ->
          Domain.apply_locks (get_exps_paths locks) astate
      (* unlock *)
      | Unlock (locks : HilExp.t list) ->
          Domain.apply_unlocks (get_exps_paths locks) astate
      (* guard construct *)
      | GuardConstruct {guard: HilExp.t; locks: HilExp.t list; strategy= Default} ->
          Domain.apply_guard_construct (get_exp_path guard) (get_exps_paths locks) ~acquire:true
            astate
      | GuardConstruct {guard: HilExp.t; locks: HilExp.t list; strategy= DeferLock | AdoptLock} ->
          Domain.apply_guard_construct (get_exp_path guard) (get_exps_paths locks) ~acquire:false
            astate
      (* guard release *)
      | GuardRelease (guard : HilExp.t) ->
          Domain.apply_guard_release (get_exp_path guard) astate
      (* guard destroy *)
      | GuardDestroy (guard : HilExp.t) ->
          Domain.apply_guard_destroy (get_exp_path guard) astate
      (* guard lock *)
      | GuardLock (guard : HilExp.t) ->
          Domain.apply_locks [get_exp_path guard] astate
      (* guard unlock *)
      | GuardUnlock (guard : HilExp.t) ->
          Domain.apply_unlocks [get_exp_path guard] astate
      (* TODO: trylock *)
      | LockedIfTrue (_ : HilExp.t list) ->
          astate
      (* TODO: guard trylock via constructor *)
      | GuardConstruct {guard: HilExp.t = _; locks: HilExp.t list = _; strategy= TryToLock} ->
          astate
      (* TODO: guard trylock *)
      | GuardLockedIfTrue (_ : HilExp.t) ->
          astate
      (* function call *)
      | NoEffect -> (
          let astate : Domain.t =
            if is_local_call_ignored analysis_data.proc_desc ~actuals then astate
            else Domain.apply_call ~f_name:(Procname.to_string callee_pname) loc astate
          in
          (* Update the abstract state with the function summary as well if it is possible. *)
          match analysis_data.analyze_dependency callee_pname with
          | Some (summary : Domain.Summary.t) ->
              Domain.apply_summary summary loc astate
          | None ->
              astate ) )
    | _ ->
        astate


  let pp_session_name (node : CFG.Node.t) (fmt : F.formatter) : unit =
    F.fprintf fmt "AtomicityViolations: %a" CFG.Node.pp_id (CFG.Node.id node)
end

(** An analyser definition. *)
module Analyser = LowerHil.MakeAbstractInterpreter (TransferFunctions (ProcCfg.Normal))

let analyse_procedure ({proc_desc} as analysis_data : Domain.Summary.t InterproceduralAnalysis.t) :
    Domain.Summary.t option =
  let pname : Procname.t = Procdesc.get_proc_name proc_desc in
  if f_is_ignored pname then None
  else
    let pre : Domain.t =
      if Procdesc.is_java_synchronized proc_desc then
        Domain.apply_locks [proc_name_to_access_path pname] Domain.initial
      else Domain.initial
    in
    (* Compute the abstract state for a given function. *)
    match Analyser.compute_post analysis_data ~initial:pre proc_desc with
    | Some (post : Domain.t) ->
        (* Convert the abstract state to the function summary. *)
        let summary : Domain.Summary.t = Domain.Summary.create post in
        (* Debug log. *)
        L.debug Analysis Verbose "\n\nFunction: %a\nAbstract State: %a\nSummary: %a\n" Procname.pp
          pname Domain.pp post Domain.Summary.pp summary ;
        Some summary
    | None ->
        L.die InternalError
          "The detection of atomicity violations failed to compute a post for '%a'." Procname.pp
          pname


let report_atomicity_violations (analysis_data : Domain.Summary.t InterproceduralAnalysis.file_t) :
    IssueLog.t =
  let summaries : (Procname.t * Domain.Summary.t) list = file_summaries analysis_data in
  let fold (issue_log : IssueLog.t) ((pname : Procname.t), (summary : Domain.Summary.t)) :
      IssueLog.t =
    if not (Domain.Summary.is_top_level_fun pname summaries) then issue_log
    else
      (* Report atomicity violations. *)
      let report (loc : Location.t) ~(msg : string) (issue_type : IssueType.t)
          (issue_log : IssueLog.t) : IssueLog.t =
        Reporting.log_issue_external pname ~issue_log ~loc
          ~ltr:[Errlog.make_trace_element 0 loc msg []]
          AtomicityViolations issue_type msg
      in
      Domain.Summary.report_atomicity_violations summary issue_log ~f:report
  in
  List.fold summaries ~init:IssueLog.empty ~f:fold
