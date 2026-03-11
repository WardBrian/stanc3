open Core
open Grace

let get_context code Middle.Location.{filename; included_from; _} =
  match (included_from, code) with
  | None, Some code ->
      (* If the location is not included from anywhere, and we have code
         provided, use it *)
      code
  | _ -> (
      (* Otherwise, by the time we are printing an error, all these files are
         already resolved. *)
      match !Include_files.include_provider with
      | FileSystemPaths _ ->
          (* So we can read directly from the filesystem *)
          In_channel.read_all filename
      | InMemory m ->
          (* Or, we know we can find it in the map *)
          Map.find_exn m filename)

let get_source ?printed_filename ?code loc =
  let code = get_context code loc in
  let name =
    if Option.is_none loc.included_from then
      Option.first_some printed_filename (Some loc.filename)
    else Some loc.filename in
  let content = String.substr_replace_all ~pattern:"\t" ~with_:" " code in
  let source : Source.t = `String {name; content} in
  source

let range_of_loc_span ?printed_filename ?code
    ({begin_loc; end_loc} : Middle.Location_span.t) =
  let source = get_source ?printed_filename ?code begin_loc in
  let max = Source.length source in
  let start =
    (* clamp handles errors at end of source *)
    Int.clamp_exn ~min:0 ~max (begin_loc.bol_offset + begin_loc.col_num) in
  let end_ =
    (* especially possible if error crosses include boundary *)
    Int.clamp_exn ~min:start ~max (end_loc.bol_offset + end_loc.col_num) in
  Range.create ~source (Byte_index.of_int start) (Byte_index.of_int end_)

let rec included_diagnostic ?printed_filename ?code
    Middle.Location.{included_from; filename; _} : Diagnostic.Label.t list =
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
      let label =
        Diagnostic.Label.secondaryf ~range:(range_of_loc loc)
          "file '%s' included here" filename in
      label :: included_diagnostic ?printed_filename ?code loc

let locate ?printed_filename ?code loc_span (diagnostic : 'a Diagnostic.t) :
    'a Diagnostic.t =
  { diagnostic with
    labels=
      diagnostic.labels
      @ Diagnostic.(
          Label.primary
            ~range:(range_of_loc_span ?printed_filename ?code loc_span)
            (Message.createf "@.%a"
               (Fmt.styled `None Message.pp)
               diagnostic.message))
        :: included_diagnostic ?printed_filename ?code loc_span.begin_loc }

open Grace_ansi_renderer

let config =
  Config.
    { default with
      styles=
        { Style_sheet.default with
          header_warning= [`Bold; `Fg `Magenta]
        ; primary_label_warning= [`Fg `Magenta]
        ; source_border= [`None; `Faint]
        ; line_number= [`Fg `Yellow] }
    ; chars= {Chars.unicode with source_border_left_break= "⋯"}
    ; num_contextual_lines= 1
    ; enable_inline_contextual_lines= true }

let pp ppf =
  let use_ansi =
    match Fmt.style_renderer ppf with `Ansi_tty -> Some true | _ -> Some false
  in
  pp_diagnostic ~config:{config with use_ansi} ?code_to_string:None ppf

let pp_compact ppf =
  let use_ansi =
    match Fmt.style_renderer ppf with `Ansi_tty -> Some true | _ -> Some false
  in
  pp_compact_diagnostic ~config:{config with use_ansi} ?code_to_string:None ppf

module Json_printer = struct
  (* Hopefully something like this will appear in Grace one day *)

  module Utf8 = struct
    (* copied from Grace_ansi_renderer *)
    let length s =
      let decoder = Uutf.decoder ~encoding:`UTF_8 (`String s) in
      let rec loop acc =
        match Uutf.decode decoder with
        | `Uchar _ -> loop (acc + 1)
        | `End -> acc
        | `Malformed _ -> raise (Invalid_argument "invalid UTF-8")
        | `Await -> assert false in
      loop 0
  end

  open Grace_source_reader

  let to_yojson ?code_to_string (d : 'a Diagnostic.t) : Yojson.Basic.t =
    with_reader @@ fun () ->
    let range_to_positions (r : Range.t) =
      let sd = open_source (Range.source r) in
      let byte_index_to_position (idx : Byte_index.t) =
        let col_of_byte_index (idx : Byte_index.t) ~sd ~line =
          (* copied from Grace_ansi_renderer *)
          let content = slicei sd (Line.start line) idx in
          let length = Utf8.length content in
          length + 1 in
        let line = Line.of_byte_index sd idx in
        let col = col_of_byte_index idx ~sd ~line in
        ((line.idx :> int) + 1, col) in
      let start_line, start_col = byte_index_to_position (Range.start r) in
      let end_line, end_col = byte_index_to_position (Range.stop r) in
      let vals =
        [ ( "start"
          , `Assoc [("line", `Int start_line); ("column", `Int start_col)] )
        ; ("end", `Assoc [("line", `Int end_line); ("column", `Int end_col)]) ]
      in
      match Source.name (Range.source r) with
      | Some name -> `Assoc (("file", `String name) :: vals)
      | None -> `Assoc vals in
    let vals =
      [ ("severity", `String (Diagnostic.Severity.to_string d.severity))
      ; ("message", `String (Diagnostic.Message.to_string d.message))
      ; ( "labels"
        , `List
            (List.map d.labels ~f:(fun label ->
                 `Assoc
                   [ ("range", range_to_positions label.range)
                   ; ( "priority"
                     , `String (Diagnostic.Priority.to_string label.priority) )
                   ; ( "message"
                     , `String (Diagnostic.Message.to_string label.message) ) ]))
        ) ] in
    match (code_to_string, d.code) with
    | Some f, Some c -> `Assoc (("error_code", `String (f c)) :: vals)
    | _ -> `Assoc vals

  let pp_json ppf d = to_yojson d |> Yojson.Basic.pretty_print ppf
end
