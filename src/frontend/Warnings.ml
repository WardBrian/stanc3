module Location_span = Middle.Location_span
module Location = Middle.Location

(* todo(grace): consider more information in warning type *)
type t = Location_span.t * string

let to_grace ?printed_filename ?code (span, message) =
  let diagnostic =
    Grace.(Diagnostic.create Warning (Diagnostic.Message.create message)) in
  Diagnostic.locate ?printed_filename ?code span diagnostic

let pp ?printed_filename ?code ppf (span, message) =
  let diagnostic = to_grace ?printed_filename ?code (span, message) in
  Fmt.pf ppf "%a@." Diagnostic.pp_compact diagnostic

let pp_warnings ?printed_filename ?code ppf warnings =
  if not (Core.List.is_empty warnings) then
    Fmt.(
      pf ppf "@[<v>%a@.@]" (list ~sep:cut (pp ?printed_filename ?code)) warnings)
