(* Author: Dominik Harmim <iharmim@fit.vut.cz> *)

open! IStd
module F = Format
module L = Logging

(** Atomicity violations analysis utilities implementation. *)

(* ************************************ Pretty Printable **************************************** *)

let pp_icollection ~(pp_item : F.formatter -> int * 'a -> unit) (fmt : F.formatter) :
    'a list -> unit =
  let pp_elements (fmt : F.formatter) (elements : 'a list) : unit =
    let pp_element (i : int) (prev : (int * 'a) option) : 'a -> (int * 'a) option =
      Option.iter prev ~f:(F.fprintf fmt "%a, " pp_item) ;
      Fn.compose Option.some (Tuple2.create i)
    in
    Option.iter (List.foldi elements ~init:None ~f:pp_element) ~f:(F.fprintf fmt "%a" pp_item)
  in
  F.fprintf fmt "{%a}" pp_elements


let pp_collection ~(pp_item : F.formatter -> 'a -> unit) : F.formatter -> 'a list -> unit =
  pp_icollection ~pp_item:(fun (fmt : F.formatter) ((_ : int), (item : 'a)) : unit ->
      pp_item fmt item )


module type PPSet = sig
  include PrettyPrintable.PPSet
end

module MakePPSet (Ord : PrettyPrintable.PrintableOrderedType) = struct
  include PrettyPrintable.MakePPSet (Ord)

  let pp (fmt : F.formatter) : t -> unit =
    Fn.compose (pp_collection ~pp_item:pp_element fmt) elements
end

module type PPMap = sig
  include PrettyPrintable.PPMap
end

module MakePPMap (Ord : PrettyPrintable.PrintableOrderedType) = struct
  include PrettyPrintable.MakePPMap (Ord)

  let pp ~(pp_value : F.formatter -> 'a -> unit) (fmt : F.formatter) : 'a t -> unit =
    Fn.compose
      (pp_collection
         ~pp_item:(fun (fmt : F.formatter) ((k : key), (v : 'a)) : unit ->
           F.fprintf fmt "%a -> %a" pp_key k pp_value v )
         fmt )
      bindings
end

(* ************************************ Modules ************************************************* *)

let assert_user (exp : bool) : ('a, F.formatter, unit) format -> 'a =
  if exp then F.ifprintf F.str_formatter else L.die UserError


module Lock = struct
  (** The bottom value of the number of times the lock has been acquired. *)
  let bot : int = 0

  (** The top value of the number of times the lock has been acquired. *)
  let top : int =
    assert_user
      (Config.atomicity_lock_level_limit > 0)
      "Input argument '--atomicity-lock-level-limit' should be greater than 0, %d given."
      Config.atomicity_lock_level_limit ;
    Config.atomicity_lock_level_limit


  type t =
    AccessPath.t
    * (int
      [@printer
        fun (fmt : F.formatter) (level : int) : unit ->
          if Int.equal level bot then F.pp_print_string fmt SpecialChars.up_tack
          else if Int.equal level top then F.pp_print_string fmt SpecialChars.down_tack
          else F.pp_print_int fmt level] )
  [@@deriving compare, equal, show {with_path= false}]

  (** Checks whether the number of times the lock has been acquired is the bottom value. *)
  let is_bot : t -> bool = Fn.compose (Int.equal bot) snd

  (** Checks whether the number of times the lock has been acquired is the top value. *)
  let is_top : t -> bool = Fn.compose (Int.equal top) snd

  let lock ((path, level) as lock : t) : t = if is_top lock then lock else (path, level + 1)

  let unlock ((path, level) as lock : t) : t = if is_bot lock then lock else (path, level - 1)

  let is_locked : t -> bool = Fn.non is_bot

  let create (path : AccessPath.t) : t = lock (path, bot)

  let path : t -> AccessPath.t = fst
end

module Guards = struct
  (** A map where a key is an access path of a lock guard. *)
  module GuardMap = MakePPMap (AccessPath)

  type t = AccessPath.t list GuardMap.t [@@deriving compare, equal]

  let pp : F.formatter -> t -> unit = GuardMap.pp ~pp_value:(pp_collection ~pp_item:AccessPath.pp)

  let empty : t = GuardMap.empty

  let add : AccessPath.t -> AccessPath.t list -> t -> t = GuardMap.add

  let remove : AccessPath.t -> t -> t = GuardMap.remove

  let reveal_locks (guards : t) : AccessPath.t list -> AccessPath.t list =
    Fn.compose List.concat
      (List.map ~f:(fun (lock : AccessPath.t) : AccessPath.t list ->
           try GuardMap.find lock guards with Caml.Not_found -> [lock] ) )
end

(* ************************************ Constants *********************************************** *)

let atomic_sets_file : string = CommandLineOption.init_work_dir ^ "/atomic-sets"

let file_comment_char : char = '#'

(* ************************************ Classes ************************************************* *)

let is_line_empty (l : string) : bool =
  Str.string_match (Str.regexp "^[ \t]*$") l 0
  || Str.string_match (Str.regexp ("^[ \t]*" ^ Char.to_string file_comment_char)) l 0


(** A class that works with names (of functions/classes/...) loaded from a file. *)
class names_from_file (file : string option) =
  object (self)
    (** Is the class initialised? *)
    val mutable initialised : bool = false

    (** A list of regular expressions that represent names loaded from a file. *)
    val mutable names : Str.regexp list = []

    (** Initialises the class. *)
    method private init : unit =
      if not initialised then (
        initialised <- true ;
        match file with
        | Some (file : string) ->
            ( match Sys.file_exists file with
            | `Yes ->
                ()
            | _ ->
                L.die UserError "File '%s' that should contain names does not exist." file ) ;
            let ic : In_channel.t = In_channel.create ~binary:false file
            and iterator (l : string) : unit =
              let l : string = String.strip l and regexp : Str.regexp = Str.regexp "^R[ \t]+" in
              if is_line_empty l then ()
              else if Str.string_match regexp l 0 then
                let pattern : string = Str.replace_first regexp "" l in
                names <- Str.regexp_case_fold (".*" ^ pattern ^ ".*") :: names
              else names <- Str.regexp_string l :: names
            in
            In_channel.iter_lines ~fix_win_eol:true ic ~f:iterator ;
            In_channel.close ic
        | None ->
            () )

    (** Checks whether the list of names is empty. *)
    method is_empty : bool =
      self#init ;
      List.is_empty names

    (** Checks whether a given name is on the list of names. *)
    method private contains (name : string) : bool =
      self#init ;
      List.exists names ~f:(fun (r : Str.regexp) : bool -> Str.string_match r name 0)

    (** Checks whether a given function is on the list of names. *)
    method contains_function : Procname.t -> bool = Fn.compose self#contains Procname.to_string

    (** Checks whether a class of a given function is on the list of names. *)
    method contains_class (pname : Procname.t) : bool =
      match Procname.base_of pname with
      | ObjC_Cpp (objc_cpp_pname : Procname.ObjC_Cpp.t) ->
          self#contains
            (QualifiedCppName.to_qual_string
               (QualifiedCppName.strip_template_args
                  (Procname.ObjC_Cpp.get_class_qualifiers objc_cpp_pname) ) )
      | _ ->
          Option.value_map (Procname.get_class_name pname) ~default:false ~f:self#contains
  end

(** An instance of the 'names_from_file' class that holds functions whose calls should be ignored. *)
let ignored_function_calls : names_from_file =
  new names_from_file Config.atomicity_ignored_function_calls_file


(** An instance of the 'names_from_file' class that holds functions whose analysis should be
    ignored. *)
let ignored_function_analyses : names_from_file =
  new names_from_file Config.atomicity_ignored_function_analyses_file


(** An instance of the 'names_from_file' class that holds functions whose calls should be allowed. *)
let allowed_function_calls : names_from_file =
  new names_from_file Config.atomicity_allowed_function_calls_file


(** An instance of the 'names_from_file' class that holds functions whose analysis should be
    allowed. *)
let allowed_function_analyses : names_from_file =
  new names_from_file Config.atomicity_allowed_function_analyses_file


(** An instance of the 'names_from_file' class that holds classes whose method calls should be
    allowed.*)
let allowed_classes : names_from_file = new names_from_file Config.atomicity_allowed_classes_file

(* ************************************ Functions *********************************************** *)

let f_is_ignored ?actuals:(actuals_opt : HilExp.t list option = None) (f : Procname.t) : bool =
  let f_string : string = Procname.to_string f
  and is_call : bool = Option.is_some actuals_opt
  and is_lock_unlock : bool =
    Option.value_map actuals_opt ~default:false ~f:(fun (actuals : HilExp.t list) : bool ->
        match ConcurrencyModels.get_lock_effect f actuals with NoEffect -> false | _ -> true )
  in
  if is_lock_unlock then false
  else if (not is_call) && ignored_function_analyses#contains_function f then true
  else if is_call && ignored_function_calls#contains_function f then true
  else if
    is_call
    && ( Procname.is_constructor f || Procname.is_destructor f
       || String.is_prefix f_string ~prefix:Config.clang_inner_destructor_prefix )
  then true
  else if
    Procname.is_cpp_assignment_operator f
    || Procname.is_java_class_initializer f
    || Procname.is_infer_undefined f
    || String.is_prefix f_string ~prefix:Config.clang_initializer_prefix
  then true
  else if String.is_prefix f_string ~prefix:"__" && BuiltinDecl.is_declared f then true
  else if
    (not is_call)
    && (not allowed_function_analyses#is_empty)
    && not (allowed_function_analyses#contains_function f)
  then true
  else if
    is_call
    && (not allowed_function_calls#is_empty)
    && not (allowed_function_calls#contains_function f)
  then true
  else if is_call && (not allowed_classes#is_empty) && not (allowed_classes#contains_class f) then
    true
  else false


let get_exps_paths : HilExp.t list -> AccessPath.t list =
  let mapper (exp : HilExp.t) : AccessPath.t =
    match HilExp.get_access_exprs exp with
    | (access_exp :: _ : HilExp.AccessExpression.t list) ->
        HilExp.AccessExpression.to_access_path access_exp
    | _ ->
        L.die InternalError "Getting an access expression for expression '%a' failed." HilExp.pp exp
  in
  List.map ~f:mapper


let get_exp_path (exp : HilExp.t) : AccessPath.t = List.hd_exn (get_exps_paths [exp])

let proc_name_to_access_path (pname : Procname.t) : AccessPath.t =
  let ident_name : string = "__atom__" ^ Procname.to_string pname in
  AccessPath.of_id (Ident.create_normal (Ident.string_to_name ident_name) 42) (Typ.mk Tvoid)


let is_local_call_ignored (proc_desc : Procdesc.t) ~(actuals : HilExp.t list) : bool =
  if not Config.atomicity_violations_ignore_local_calls then false
  else
    match actuals with
    | (exp :: _ : HilExp.t list) ->
        let formals : Pvar.t list = List.map (Procdesc.get_pvar_formals proc_desc) ~f:fst in
        List.for_all (HilExp.get_access_exprs exp)
          ~f:(fun (access_exp : HilExp.AccessExpression.t) : bool ->
            match HilExp.AccessExpression.get_base access_exp with
            | (ProgramVar (pv : Pvar.t) : Var.t), (_ : Typ.t) ->
                (Pvar.is_local pv || Pvar.is_static_local pv)
                && (not (Pvar.is_this pv))
                && not (List.mem formals pv ~equal:Pvar.equal)
            | _ ->
                false )
    | _ ->
        false


let file_summaries (analysis_data : 'a InterproceduralAnalysis.file_t) : (Procname.t * 'a) list =
  List.filter_map analysis_data.procedures
    ~f:(fun (pname : Procname.t) : (Procname.t * 'a) option ->
      Option.value_map
        (analysis_data.analyze_file_dependency pname)
        ~default:None
        ~f:(Fn.compose Option.some (Tuple2.create pname)) )
