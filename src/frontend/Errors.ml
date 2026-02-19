(** Setup of our compiler errors *)

open Core

type t =
  | FileNotFound of string
  | Syntax_error of Syntax_error.t
  | Semantic_error of Semantic_error.t
  | DebugDataError of (Middle.Location_span.t * string * bool)

let get_context ?code Middle.Location.{filename; included_from; _} =
  Option.try_with @@ fun () ->
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

let red = Fmt.(styled `Bold (styled (`Fg `Red) string))

let get_source ?code loc =
  let code = get_context ?code loc |> Option.value ~default:[] in
  let source : Grace.Source.t =
    `String {name= Some loc.filename; content= code |> String.concat ~sep:"\n"}
  in
  source

let range_of_loc_span ?code ({begin_loc; end_loc} : Middle.Location_span.t) =
  let open Grace in
  let source = get_source ?code begin_loc in
  let start = begin_loc.bol_offset + begin_loc.col_num in
  let end_ = end_loc.bol_offset + end_loc.col_num in
  let end_ = Int.clamp_exn ~min:start ~max:(Source.length source) end_ in
  Range.create ~source (Byte_index.of_int start) (Byte_index.of_int end_)

let rec included_diagnostic Middle.Location.{included_from; filename; _} :
    Grace.Diagnostic.Label.t list =
  let open Grace in
  let range_of_loc loc =
    let source = get_source loc in
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
      label :: included_diagnostic loc

let syntax_error_to_grace ?code err =
  let loc_span = Syntax_error.location err in
  Grace.Diagnostic.(
    (* We'd need more locations to really do much -- hard from parser, easy from
       typechecker *)
    createf
      ~labels:
        Label.(
          [ primaryf
              ~range:(range_of_loc_span ?code loc_span)
              "%a" Syntax_error.pp err ]
          @ included_diagnostic loc_span.begin_loc)
      Error "%a" Syntax_error.pp err)

let semantic_error_to_grace ?code err =
  let loc_span = Semantic_error.location err in
  Grace.Diagnostic.(
    createf
      ~labels:
        Label.(
          [ primaryf
              ~range:(range_of_loc_span ?code loc_span)
              "%a" Semantic_error.pp err ]
          @ included_diagnostic loc_span.begin_loc)
      Error "%a" Semantic_error.pp err)

let pp_diagnostic ppf diagnostic =
  Fmt.pf ppf "%a@."
    (Grace_ansi_renderer.pp_diagnostic ?config:None ?code_to_string:None)
    diagnostic

let pp ?printed_filename ?code ppf = function
  | FileNotFound f ->
      Fmt.pf ppf "%a: file '%s' not found or cannot be opened@." red "Error" f
  | Syntax_error err -> pp_diagnostic ppf (syntax_error_to_grace ?code err)
  | Semantic_error err -> pp_diagnostic ppf (semantic_error_to_grace ?code err)
  | DebugDataError (loc, msg, had_context) ->
      if Middle.Location_span.(compare loc empty = 0) then
        Fmt.pf ppf "%a: %s" red "Error" msg
      else
        Fmt.pf ppf "@[<v2>%a in %a:@ %s%a@.@]" red "Error"
          (Middle.Location_span.pp ?printed_filename)
          loc msg
          (Fmt.if' (not had_context)
             (Fmt.any "@ Supplying a --debug-data-file may help"))
          ()
