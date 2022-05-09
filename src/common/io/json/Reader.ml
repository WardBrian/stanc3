open Core_kernel
open Js_of_ocaml

class type buffer =
  object
    method toString : Js.js_string Js.t Js.meth
  end

class type fs =
  object
    method readFileSync : Js.js_string Js.t -> buffer Js.t Js.meth
  end

let require_module s =
  Js.Unsafe.fun_call
    (Js.Unsafe.js_expr "require")
    [|Js.Unsafe.inject (Js.string s)|]

let try_process_file (callback : string -> 'a) fname =
  try
    let fs : fs Js.t = require_module "fs" in
    (fs##readFileSync (Js.string fname))##toString
    |> Js.to_string |> callback |> Option.some
  with _ -> None

let read_lines = try_process_file String.split_lines
let lexbuf_of = try_process_file Lexing.from_string
