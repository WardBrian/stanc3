open Ast
open Std

let unused_list pred id anns =
  List.filter_map
    ~f:(fun a ->
      if pred a then None else Some (Warnings.unknown_annotation (id.id_loc, a)))
    anns

let rec collect_stmt pred (acc : Warnings.t list) {stmt; _} =
  match stmt with
  | FunDef {annotations; funname; _} ->
      acc @ (unused_list pred funname) annotations
  | VarDecl {annotations; variables; _} ->
      acc @ (unused_list pred (List.hd_exn variables).identifier) annotations
  | _ -> fold_statement Fun.const (collect_stmt pred) Fun.const acc stmt

let find_unrecognized (pred : string -> bool) (prog : Ast.untyped_program) :
    Warnings.t list =
  fold_program (collect_stmt pred) [] prog
