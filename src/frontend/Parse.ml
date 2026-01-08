(** Some complicated stuff to get the custom syntax errors out of Menhir's
    Incremental API *)

open Core
open Common.Let_syntax.Result
module Interp = Parser.MenhirInterpreter

let remember (type a b) (next : a -> b) : (a -> b) * (unit -> b) =
  let cell : b option ref = ref None in
  let next a =
    let b = next a in
    cell := Some b;
    b
  and last () =
    match !cell with
    | Some b -> b
    | None ->
        (* [last] must not be called until [next] has been called once. *)
        assert false in
  (next, last)

let drive_parser parse_fun =
  let input () =
    Interp.lexer_lexbuf_to_supplier Lexer.token
      (Preprocessor.current_buffer ())
      () in
  (* annoying, wish we just had a version of Interp.positions that gave the
     whole triple *)
  let input, last = remember input in
  let success prog = {prog with Ast.comments= Preprocessor.get_comments ()} in
  let failure previous_state error_state =
    (* see the Menhir manual for the description of error messages support *)
    let env =
      match error_state with
      | Interp.HandlingError env -> env
      | _ ->
          Common.ICE.internal_compiler_error
            [%message "Parser failed but is not in an error state"] in
    let prev_env =
      match previous_state with
      | Interp.InputNeeded env -> env
      | _ ->
          Common.ICE.internal_compiler_error
            [%message "Parser failed but was not previously expecting input"]
    in
    let triple = last () in
    (* lrgrep's error_message takes _prev_env and triple

       https://cambium.inria.fr/~fpottier/publis/bour-pottier-lrgrep-2026.pdf
       $5 *)
    let message =
      let state = Interp.current_state_number env in
      try
        Option.value_exn (Parser_errors.error_message prev_env triple)
        ^^
        if !Debugging.grammar_logging then
          Scanf.format_from_string
            ("(Parse error state " ^ string_of_int state ^ ")")
            ""
        else ""
      with _ ->
        Scanf.format_from_string
            ("Syntax error (Parse error state " ^ string_of_int state ^ ")\n") "" in
    let location =
      let _token, s, e = triple in
      Preprocessor.location_span_of_positions (s, e) in
    Syntax_error.parse_error message location in
  let startp = (Preprocessor.current_buffer ()).lex_curr_p in
  Syntax_error.try_with (fun () ->
      Interp.loop_handle_undo success failure input (parse_fun startp))

let to_lexbuf file_or_code =
  match file_or_code with
  | `File path ->
      let+ chan =
        try Ok (In_channel.create path)
        with _ -> Error (Errors.FileNotFound path) in
      (Lexing.from_channel chan, path)
  | `Code code -> Ok (Lexing.from_string code, "string")

let parse parse_fun file_or_code =
  Input_warnings.init ();
  let result =
    let* lexbuf, name = to_lexbuf file_or_code in
    Preprocessor.init lexbuf name;
    drive_parser parse_fun
    |> Result.map_error ~f:(fun e -> Errors.Syntax_error e) in
  (result, Input_warnings.collect ())

let parse_stanfunctions file_or_code =
  parse Parser.Incremental.functions_only file_or_code

let parse_program file_or_code = parse Parser.Incremental.program file_or_code
