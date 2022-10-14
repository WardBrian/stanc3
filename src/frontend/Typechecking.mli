(** a type/semantic checker for Stan ASTs

  Functions which begin with "check_" return a typed version of their input
  Functions which begin with "verify_" return unit if a check succeeds, or else
    throw an [Errors.SemanticError] exception.
  Other functions which begin with "infer"/"calculate" vary. Usually they return
    a value, but a few do have error conditions.

  All [Error.SemanticError] excpetions are caught by check_program
  which turns the ast or exception into a [Result.t] for external usage

  A type environment {!val:Environment.t} is used to hold variables and functions, including
  Stan math functions. This is a functional map, meaning it is handled immutably.

  This module is parameterized over a Standard Library of function signatures, See
  [Std_library_utils.Library]. For the main compiler, this is
  [Stan_math_backend.Stan_math_library]
*)

open Ast
open Typechecking_intf

val model_name : string ref
(** A reference to hold the model name. Relevant for checking variable
      clashes and used in code generation. *)

val check_that_all_functions_have_definition : bool ref
(** A switch to determine whether we check that all functions have a definition *)

val get_arg_types : typed_expression list -> Std_library_utils.fun_arg list
val type_of_expr_typed : typed_expression -> Middle.UnsizedType.t

val calculate_autodifftype :
     Environment.originblock
  -> Environment.originblock
  -> Middle.UnsizedType.t
  -> Middle.UnsizedType.autodifftype

val make_function_variable :
     Environment.originblock
  -> Middle.Location_span.t
  -> identifier
  -> Middle.UnsizedType.t
  -> Ast.typed_expression

module Make (StdLibrary : Std_library_utils.Library) : TYPECHECKER
