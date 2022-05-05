open Core_kernel

let read fname = try In_channel.read_lines fname |> Option.some with _ -> None
