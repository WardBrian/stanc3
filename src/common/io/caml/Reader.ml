open Core_kernel

let read_lines fname =
  try In_channel.read_lines fname |> Option.some with _ -> None

let lexbuf_of fname =
  try In_channel.create fname |> Lexing.from_channel |> Option.some
  with _ -> None
