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

let read fname =
  try
    let fs : fs Js.t = require_module "fs" in
    (fs##readFileSync (Js.string fname))##toString
    |> Js.to_string |> String.split_lines |> Option.some
  with _ -> None
