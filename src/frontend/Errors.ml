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

(* let pp_context ?code ppf loc = let context = get_context ?code loc |>
   Option.map ~f:(fun lines -> (loc, Array.of_list lines)) in (Fmt.option
   Middle.Location.pp_context_for) ppf context *)

let red = Fmt.(styled `Bold (styled (`Fg `Red) string))

type code = Syntax_error | Semantic_error

let code_to_string = function
  | Syntax_error -> "Syntax error"
  | Semantic_error -> "Semantic error"

let range_of_loc (loc_span : Middle.Location_span.t) =
  let open Grace in
  let code =
    get_context ?code:None loc_span.begin_loc |> Option.value ~default:[] in
  let source : Source.t =
    `String
      { name= Some loc_span.begin_loc.filename
      ; content= code |> String.concat ~sep:"\n" } in
  (* yuck! would want to just store as byte offset, which is what lexer
     provides, imo *)
  let bol n =
    List.foldi code ~init:0 ~f:(fun i acc line ->
        if i < n - 1 then acc + String.length line + 1 else acc) in
  let start = bol loc_span.begin_loc.line_num + loc_span.begin_loc.col_num in
  let stop = bol loc_span.end_loc.line_num + loc_span.end_loc.col_num in
  Range.create ~source (Byte_index.of_int start) (Byte_index.of_int stop)

let syntax_error_to_grace err =
  let loc_span = Syntax_error.location err in
  Grace.Diagnostic.(
    (* We'd need more locations to really do much -- hard from parser, easy from
       typechecker *)
    createf
      ~labels:
        Label.[primaryf ~range:(range_of_loc loc_span) "%a" Syntax_error.pp err]
      ~code:Syntax_error Error "%a" Syntax_error.pp err)

let semantic_error_to_grace err =
  let loc_span = Semantic_error.location err in
  Grace.Diagnostic.(
    createf
      ~labels:
        Label.
          [ primaryf ~range:(range_of_loc loc_span) "%a"
              (Fmt.styled `None Semantic_error.pp)
              err ]
      ~code:Semantic_error Error "%a" Semantic_error.pp err)

let pp ?printed_filename ?code:_ ppf = function
  | FileNotFound f ->
      Fmt.pf ppf "%a: file '%s' not found or cannot be opened@." red "Error" f
  | Syntax_error err ->
      (* let loc_span = Syntax_error.location err in let error_type =
         Syntax_error.kind err in Fmt.pf ppf "%a in %a, %s:@;%a@,%a" red "Syntax
         error" (Middle.Location_span.pp ?printed_filename) loc_span error_type
         (pp_context ?code) loc_span.begin_loc Syntax_error.pp err *)
      Fmt.pf ppf "%a@."
        (Grace_ansi_renderer.pp_diagnostic ?config:None ~code_to_string)
        (syntax_error_to_grace err)
  | Semantic_error err ->
      (* let loc_span = Semantic_error.location err in Fmt.pf ppf "%a in
         %a:@;%a@,%a@." red "Semantic error" (Middle.Location_span.pp
         ?printed_filename) loc_span (pp_context ?code) loc_span.begin_loc
         Semantic_error.pp err *)
      Fmt.pf ppf "%a@."
        (Grace_ansi_renderer.pp_diagnostic ?config:None ~code_to_string)
        (semantic_error_to_grace err)
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
