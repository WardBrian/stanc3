(** Some complicated stuff to get the custom syntax errors out of Menhir's Incremental
    API *)

open Core
open Common.Let_syntax.Result

let drive_parser parse_fun =
  let lex in_buf =
    let real_buf = Preprocessor.current_buffer () in
    let tok = Lexer.token real_buf in
    (* TODO -- not really working, I think this needs to check if the tokenizer
      changed buffers due to #includes *)
    in_buf.Lexing.lex_start_p <- (Preprocessor.current_buffer ()).lex_start_p;
    in_buf.Lexing.lex_curr_p <- real_buf.lex_curr_p;
    tok in
  try
    let prog = parse_fun lex (Preprocessor.current_buffer ()) in
    Ok {prog with Ast.comments= Preprocessor.get_comments ()}
  with
  | Errors.SyntaxError err -> Result.Error (Errors.Syntax_error err)
  | Parser.Error state ->
      let message =
        try
          Fmt.str "%s%a"
            (Parsing_errors.message state)
            (Fmt.if' !Debugging.grammar_logging (fun ppf ->
                 Fmt.pf ppf "(Parse error state %d)"))
            state
        with _ ->
          Common.ICE.internal_compiler_error
            [%message
              "Failed to find error for parser error state " (state : int)]
      in
      let location =
        Preprocessor.location_span_of_positions
          ( Lexing.lexeme_start_p (Preprocessor.current_buffer ())
          , Lexing.lexeme_end_p (Preprocessor.current_buffer ()) ) in
      Error (Errors.Syntax_error (Errors.Parsing (message, location)))

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
    drive_parser parse_fun in
  (result, Input_warnings.collect ())

let parse_stanfunctions file_or_code = parse Parser.functions_only file_or_code
let parse_program file_or_code = parse Parser.program file_or_code
