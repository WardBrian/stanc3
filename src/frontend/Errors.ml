(** Setup of our compiler errors *)

open Core

type t =
  | FileNotFound of string
  | Syntax_error of Syntax_error.t
  | Semantic_error of Semantic_error.t
  | DebugDataError of (Middle.Location_span.t * string * bool)

let get_context ?code Middle.Location.{filename; included_from; _} =
  match (included_from, code) with
  | None, Some code ->
      (* If the location is not included from anywhere, and we have code
         provided, use it *)
      String.split_lines code
  | _ -> (
      (* Otherwise, by the time we are printing an error, all these files are
         already resolved. *)
      match !Include_files.include_provider with
      | FileSystemPaths _ ->
          (* So we can read directly from the filesystem *)
          In_channel.read_lines filename
      | InMemory m ->
          (* Or, we know we can find it in the map *)
          String.split_lines (Map.find_exn m filename))

let get_source ?printed_filename ?code loc =
  let code = get_context ?code loc in
  let source : Grace.Source.t =
    `String
      { name=
          (if Option.is_none loc.included_from then
             Option.first_some printed_filename (Some loc.filename)
           else Some loc.filename)
      ; content= code |> String.concat ~sep:"\n" } in
  source

let range_of_loc_span ?printed_filename ?code
    ({begin_loc; end_loc} : Middle.Location_span.t) =
  let open Grace in
  let source = get_source ?printed_filename ?code begin_loc in
  let start = begin_loc.bol_offset + begin_loc.col_num in
  let end_ = end_loc.bol_offset + end_loc.col_num in
  let end_ = Int.clamp_exn ~min:start ~max:(Source.length source) end_ in
  Range.create ~source (Byte_index.of_int start) (Byte_index.of_int end_)

let rec included_diagnostic ?printed_filename ?code
    Middle.Location.{included_from; filename; _} : Grace.Diagnostic.Label.t list
    =
  let open Grace in
  let range_of_loc loc =
    let source = get_source ?printed_filename ?code loc in
    let start = loc.bol_offset + loc.col_num in
    let end_ =
      start + 8
      (* '#include' *) in
    let end_ = Int.clamp_exn ~min:start ~max:(Source.length source) end_ in
    Range.create ~source (Byte_index.of_int start) (Byte_index.of_int end_)
  in
  match included_from with
  | None -> []
  | Some loc ->
      let open Grace in
      let label =
        Diagnostic.Label.secondaryf ~range:(range_of_loc loc)
          "file '%s' included here" filename in
      label :: included_diagnostic ?printed_filename ?code loc

let syntax_error_to_grace ?printed_filename ?code err =
  let loc_span = Syntax_error.location err in
  let diagnostic =
    Grace.Diagnostic.(
      (* We'd need more locations to really do much -- hard from parser, easy
         from typechecker *)
      createf
        ~labels:
          Label.
            [ primaryf
                ~range:(range_of_loc_span ?printed_filename ?code loc_span)
                "%a" Syntax_error.pp err ]
        Error "%a" Syntax_error.pp err) in
  { diagnostic with
    labels=
      diagnostic.labels
      @ included_diagnostic ?printed_filename ?code loc_span.begin_loc }

let semantic_error_to_grace ?printed_filename ?code err =
  let loc_span = Semantic_error.location err in
  let diagnostic =
    Grace.Diagnostic.(
      createf
        ~labels:
          Label.
            [ primaryf
                ~range:(range_of_loc_span ?printed_filename ?code loc_span)
                "%a" Semantic_error.pp err ]
        Error "%a" Semantic_error.pp err) in
  { diagnostic with
    labels=
      diagnostic.labels
      @ included_diagnostic ?printed_filename ?code loc_span.begin_loc }

let pp ?printed_filename ?code ppf t =
  let diagnostic =
    let open Grace.Diagnostic in
    match t with
    | FileNotFound f ->
        createf Error "file '%s' not found or cannot be opened" f
    | Syntax_error err -> syntax_error_to_grace ?printed_filename ?code err
    | Semantic_error err -> semantic_error_to_grace ?printed_filename ?code err
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
  Fmt.pf ppf "%a@."
    (Grace_ansi_renderer.pp_diagnostic ?config:None ?code_to_string:None)
    diagnostic
