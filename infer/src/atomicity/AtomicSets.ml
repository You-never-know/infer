(* Author: Dominik Harmim <iharmim@fit.vut.cz> *)

open! IStd
open AtomicityUtils
module Domain = AtomicSetsDomain
module F = Format
module L = Logging

(** Detection of atomic sets implementation. *)

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
            Domain.apply_call ~f_name:(Procname.to_string callee_pname) loc astate
          in
          (* Update the abstract state with the function summary as well if it is possible. *)
          match analysis_data.analyze_dependency callee_pname with
          | Some (summary : Domain.Summary.t) ->
              Domain.apply_summary summary astate
          | None ->
              astate ) )
    | _ ->
        astate


  let pp_session_name (node : CFG.Node.t) (fmt : F.formatter) : unit =
    F.fprintf fmt "AtomicSets: %a" CFG.Node.pp_id (CFG.Node.id node)
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
        (* Update the abstract state at the end of a function and convert the abstract state to the
           function summary. *)
        let updated_post : Domain.t = Domain.final_update post in
        let summary : Domain.Summary.t = Domain.Summary.create updated_post in
        (* Debug log. *)
        L.debug Analysis Verbose "\n\nFunction: %a\nAbstract State: %a\nSummary: %a\n" Procname.pp
          pname Domain.pp updated_post Domain.Summary.pp summary ;
        Some summary
    | None ->
        L.die InternalError "The detection of atomic sets failed to compute a post for '%a'."
          Procname.pp pname


let print_atomic_sets (analysis_data : Domain.Summary.t InterproceduralAnalysis.file_t) : IssueLog.t
    =
  (* Print to a file. *)
  let oc : Out_channel.t =
    Out_channel.create ~binary:false ~append:Config.atomic_sets_file_append atomic_sets_file
  and procedures_count : int ref = ref 0
  and atomic_sets_count : int ref = ref 0
  and atomic_functions_count : int ref = ref 0 in
  let summaries : (Procname.t * Domain.Summary.t) list = file_summaries analysis_data in
  let print_atomic_sets ((pname : Procname.t), (summary : Domain.Summary.t)) : unit =
    let (atomic_sets_count' : int), (atomic_functions_count' : int) =
      Domain.Summary.print_atomic_sets ~f_name:(Procname.to_string pname) oc summaries summary
    in
    if not (Int.equal atomic_sets_count' 0) then incr procedures_count ;
    atomic_sets_count := atomic_sets_count' + !atomic_sets_count ;
    atomic_functions_count := atomic_functions_count' + !atomic_functions_count
  in
  List.iter summaries ~f:print_atomic_sets ;
  (* Print stats. *)
  if not (Int.equal !procedures_count 0) then Out_channel.newline oc ;
  Out_channel.fprintf oc
    "%c Number of (analysed functions; atomic sets; atomic functions): (%i; %i; %i)\n"
    file_comment_char !procedures_count !atomic_sets_count !atomic_functions_count ;
  Out_channel.close oc ;
  F.fprintf F.std_formatter "The detection of atomic sets produced an output into the file '%s'.\n"
    atomic_sets_file ;
  IssueLog.empty
