open Mir
open Core_kernel

let stan_math_map =
  String.Map.of_alist_exn
    ["+", "add"; "-", "minus"; "/", "divide"; "*", "multiply"]

let rec translate_fn_names = function
  | FnApp(f, args) -> FnApp(
      String.Map.find stan_math_map f |> Option.value ~default:f,
      List.map ~f:translate_fn_names args)
  | x -> x

exception ShouldNeverHappen

let rec emit_cpp = function
  | FnApp(f, args) ->
    let args = List.map ~f:emit_cpp args in
    begin match f with
      (* XXX Choose one loop type!*)
      | "if" | "for" | "while" ->
        begin match args with
          | fst :: snd :: thrd ->
            (sprintf "%s (%s) {\n  %s;\n}%s"
               f fst snd (List.fold ~init:""
                            ~f:(fun _ e -> sprintf " else {\n  %s;\n}\n" e)
                            thrd))
      (* XXX Is this non-typesafety too ugly? Alternative here is probably to
         just have the MIR have a separate variant for loop, if, and ifelse.
         ... that seems better...
      *)
          | _ -> raise ShouldNeverHappen
        end
      | f -> String.concat [f; "("; (String.concat ~sep:", " args); ")"]
    end
  | Var(v) -> v
  | Lit(Str, s) -> String.concat ["\""; s; "\""]
  | Lit(_, v) -> v
  | ExprList(l) -> String.concat ~sep:"; " (List.map ~f:emit_cpp l)
  | AssignExpr(rhs, lhs) -> String.concat [rhs; " = "; emit_cpp lhs]

let prog_reader_call path =
  (* open the file and see how long it is and shit *)
  let


let build_header fname path =
  ["// Code generated by Stan version ";
   version;
   "\n#include <stan/model/model_header.hpp>\nnamespace "; fname;
   {|
using std::istream;
using std::string;
using std::stringstream;
using std::vector;
using stan::io::dump;
using stan::math::lgamma;
using stan::model::prob_grad;
using namespace stan::math;

static int current_statement_begin__;

stan::io::program_reader prog_reader__() {
    stan::io::program_reader reader;
|};
   "    reader.add_event(0, 0, \"start\", "; path; ");";
   "    reader.add_event("0, 0, \"end\", "; path; ");";

  ]


let emit fname e =
  String.concat
    build_header fname ::
  [e
     |> translate_fn_names
     |> emit_cpp
    ]

let%expect_test "emit" =
  let e = FnApp("+", [Lit(Int, "1"); Lit(Real, "3.2")]) in
  emit e |> print_endline;
  [%expect {| add(1, 3.2) |}]

let%expect_test "emitif" =
  let e = FnApp("if", [Lit(Int, "1"); Lit(Real, "3.2")]) in
  emit e |> print_endline;
  [%expect {|
    if (1) {
      3.2;
    } |}]

let%expect_test "emitifelse" =
  let e = FnApp("if", [Lit(Int, "1"); Lit(Real, "3.2"); Lit(Real, "44.3")]) in
  emit e |> print_endline;
  [%expect {|
    if (1) {
      3.2;
    } else {
      44.3;
    } |}]