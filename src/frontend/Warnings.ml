module Location_span = Middle.Location_span
module Location = Middle.Location

type t = Location_span.t * string

let pp ?printed_filename ?code ppf (span, message) =
  let diagnostic =
    Grace.(Diagnostic.create Warning (Diagnostic.Message.create message)) in
  let diagnostic = Diagnostic.locate ?printed_filename ?code span diagnostic in
  Diagnostic.pp ppf diagnostic

let pp_warnings ?printed_filename ?code ppf warnings =
  if not (Core.List.is_empty warnings) then
    Fmt.(pf ppf "@[<v>%a@.@]" (list ~sep:cut (pp ?printed_filename ?code)) warnings)
