(*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 *)

open! IStd
module F = Format
module L = Logging

let dummy = "dummy.py"

let test ?(filename = dummy) source =
  if not (Py.is_initialized ()) then Py.initialize ~interpreter:Version.python_exe () ;
  ( match FFI.from_string ~source ~filename with
  | Error (kind, err) ->
      L.die kind "FFI error: %a@\n" FFI.Error.pp_kind err
  | Ok code ->
      F.printf "%s" (FFI.Code.show code)
  | exception (Py.E _ as e) ->
      L.die ExternalError "Pyml exception: %s@\n" (Exn.to_string e) ) ;
  Py.finalize ()


let%test_module "load_code" =
  ( module struct
    let%expect_test _ =
      let source = "x = 42" in
      test source ;
      [%expect
        {|
        { co_name = "<module>"; co_firstlineno = 1; co_filename = "dummy.py";
          co_flags = 64; co_cellvars = [||]; co_freevars = [||]; co_names = [|"x"|];
          co_varnames = [||]; co_nlocals = 0; co_argcount = 0;
          co_posonlyargcount = 0; co_stacksize = 1; co_kwonlyargcount = 0;
          co_lnotab = [||]; co_consts = [|42; None|];
          instructions =
          [   1        0 LOAD_CONST                        0 (42);
                       2 STORE_NAME                        0 (x);
                       4 LOAD_CONST                        1 (None);
                       6 RETURN_VALUE                      0 ];
          version = python3.10 } |}]


    let%expect_test _ =
      let source = "print(5j)" in
      test source ;
      [%expect
        {|
        { co_name = "<module>"; co_firstlineno = 1; co_filename = "dummy.py";
          co_flags = 64; co_cellvars = [||]; co_freevars = [||];
          co_names = [|"print"|]; co_varnames = [||]; co_nlocals = 0;
          co_argcount = 0; co_posonlyargcount = 0; co_stacksize = 2;
          co_kwonlyargcount = 0; co_lnotab = [||];
          co_consts = [|Complex[real:0.000000; imag:5.000000 ]; None|];
          instructions =
          [   1        0 LOAD_NAME                         0 (print);
                       2 LOAD_CONST                        0 (5j);
                       4 CALL_FUNCTION                     1 ;
                       6 POP_TOP                           0 ;
                       8 LOAD_CONST                        1 (None);
                      10 RETURN_VALUE                      0 ];
          version = python3.10 } |}]


    let%expect_test _ =
      let source = "x = ..." in
      test source ;
      [%expect
        {|
        { co_name = "<module>"; co_firstlineno = 1; co_filename = "dummy.py";
          co_flags = 64; co_cellvars = [||]; co_freevars = [||]; co_names = [|"x"|];
          co_varnames = [||]; co_nlocals = 0; co_argcount = 0;
          co_posonlyargcount = 0; co_stacksize = 1; co_kwonlyargcount = 0;
          co_lnotab = [||]; co_consts = [|None; None|];
          instructions =
          [   1        0 LOAD_CONST                        0 (Ellipsis);
                       2 STORE_NAME                        0 (x);
                       4 LOAD_CONST                        1 (None);
                       6 RETURN_VALUE                      0 ];
          version = python3.10 } |}]


    let%expect_test _ =
      let source = "x = 100000000000000000000" in
      test source ;
      [%expect
        {|
        { co_name = "<module>"; co_firstlineno = 1; co_filename = "dummy.py";
          co_flags = 64; co_cellvars = [||]; co_freevars = [||]; co_names = [|"x"|];
          co_varnames = [||]; co_nlocals = 0; co_argcount = 0;
          co_posonlyargcount = 0; co_stacksize = 1; co_kwonlyargcount = 0;
          co_lnotab = [||]; co_consts = [|100000000000000000000; None|];
          instructions =
          [   1        0 LOAD_CONST                        0 (100000000000000000000);
                       2 STORE_NAME                        0 (x);
                       4 LOAD_CONST                        1 (None);
                       6 RETURN_VALUE                      0 ];
          version = python3.10 } |}]
  end )
