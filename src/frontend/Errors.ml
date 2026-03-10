(** Setup of our compiler errors *)

open Core
open Diagnostic

type t =
  | FileNotFound of string
  | Syntax_error of Syntax_error.t
  | Semantic_error of Semantic_error.t
  | DebugDataError of (Middle.Location_span.t * string * bool)

let syntax_error_to_grace ?printed_filename ?code err =
  let loc_span = Syntax_error.location err in
  let diagnostic =
    Grace.Diagnostic.(
      (* todo(grace): to really do much better, we need to split up messages
         from the parser somehow *)
      createf
        ~notes:(Syntax_error.notes err |> List.map ~f:Message.create)
        Error "%a" Syntax_error.pp err) in
  locate ?printed_filename ?code loc_span diagnostic

let pp ?printed_filename ?code ppf t =
  let diagnostic =
    let open Grace.Diagnostic in
    match t with
    | FileNotFound f ->
        createf Error "file '%s' not found or cannot be opened" f
    | Syntax_error err -> syntax_error_to_grace ?printed_filename ?code err
    | Semantic_error err -> Semantic_error.to_grace ?printed_filename ?code err
    | DebugDataError (loc, msg, had_context) ->
        (* todo -- try to parse yojson message back into a location? *)
        let notes =
          if had_context then []
          else [Message.create "Supplying a --debug-data-file may help."] in
        let labels =
          if Middle.Location_span.(compare loc empty = 0) then []
          else
            [ Label.primaryf
                ~range:(range_of_loc_span ?printed_filename ?code loc)
                "here" ] in
        createf ~labels ~notes Error "%s" msg in
  Fmt.pf ppf "%a@." Diagnostic.pp diagnostic
