open Core_kernel
open! Core_kernel.Poly
open Middle
open! Lower_expr
open! Lower_stmt
open Cpp

(** This function produces the function and any functor definitions.
    Functor declarations need to be collated, and are therefore stored in the
    functors hashtable *)
let lower_fun_def (functors : (string, fun_defn list) Hashtbl.t)
    (forward_decls : (string * template_parameter list) Hash_set.t)
    (funs_used_in_reduce_sum : String.Set.t)
    (funs_used_in_variadic_ode : String.Set.t)
    (funs_used_in_variadic_dae : String.Set.t)
    Program.{fdrt; fdname; fdsuffix; fdargs; fdbody; _} : fun_defn list =
  Caml.ignore
    ( functors
    , forward_decls
    , fdrt
    , fdname
    , fdsuffix
    , fdargs
    , fdbody
    , funs_used_in_reduce_sum
    , funs_used_in_variadic_dae
    , funs_used_in_variadic_ode ) ;
  failwith "TODO"

let is_fun_used_with_variadic_fn (variadic_fn_test : string -> bool)
    (p : Program.Numbered.t) =
  let rec find_functors_expr accum Expr.Fixed.{pattern; _} =
    String.Set.union accum
      ( match pattern with
      | FunApp (StanLib (x, FnPlain, _), {pattern= Var f; _} :: _)
        when variadic_fn_test x ->
          String.Set.of_list [Utils.stdlib_distribution_name f]
      | x -> Expr.Fixed.Pattern.fold find_functors_expr accum x ) in
  let rec find_functors_stmt accum stmt =
    Stmt.Fixed.(
      Pattern.fold find_functors_expr find_functors_stmt accum stmt.pattern)
  in
  Program.fold find_functors_expr find_functors_stmt String.Set.empty p

(** We need to do a fair bit of bookkeeping to handle the functors necessary for the various
    higher order functions.

    Each functor needs a forward decl struct before the function,
    then the function definition,
    then the actual functor definitions
  *)
let collect_functors_functions (p : Program.Numbered.t) : defn list =
  let (functors : (string, Cpp.fun_defn list) Hashtbl.t) =
    String.Table.create () in
  let forward_decls = Hash_set.Poly.create () in
  let reduce_sum_fns =
    is_fun_used_with_variadic_fn Stan_math_signatures.is_reduce_sum_fn p in
  let variadic_ode_fns =
    is_fun_used_with_variadic_fn Stan_math_signatures.is_variadic_ode_fn p in
  let variadic_dae_fns =
    is_fun_used_with_variadic_fn Stan_math_signatures.is_variadic_dae_fn p in
  let to_defns funs = List.map ~f:(fun f -> FunDef f) funs in
  let fun_and_functor_defs =
    p.functions_block
    |> List.concat_map
         ~f:
           (lower_fun_def functors forward_decls reduce_sum_fns variadic_ode_fns
              variadic_dae_fns )
    |> to_defns in
  let functor_struct_decls =
    functors |> Hashtbl.to_alist
    |> List.map ~f:(fun (name, fun_defns) ->
           Struct
             (make_struct_defn ~param:(*TODO*) None ~name
                ~body:(to_defns fun_defns) () ) ) in
  functor_struct_decls @ fun_and_functor_defs
