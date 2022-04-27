(** This module stores a table of all signatures from the Stan
    math C++ library which are exposed to Stan, and some helper
    functions for dealing with those signatures.
*)

open Frontend
open Middle
open Std_library_utils
open Core_kernel

val function_signatures : (string, signature list) Hashtbl.t
(** Mapping from names to signature(s) of functions *)

val distribution_families : string list

val is_stdlib_function_name : string -> bool
(** Equivalent to [Hashtbl.mem function_signatures s]*)

val get_signatures : string -> signature list
(** Equivalent to [Hashtbl.find_multi function_signatures s]*)

val get_operator_signatures : Operator.t -> signature list
val get_assignment_operator_signatures : Operator.t -> signature list
val is_not_overloadable : string -> bool
val is_variadic_function_name : string -> bool
val variadic_function_returntype : string -> UnsizedType.returntype option

val check_variadic_fn :
     Ast.identifier
  -> is_cond_dist:bool
  -> Location_span.t
  -> Environment.originblock
  -> Environment.t
  -> Ast.typed_expression list
  -> Ast.typed_expression
(** This function is responsible for typechecking varadic function
      calls. It needs to live in the Library since this is usually
      bespoke per-function. *)

val operator_to_function_names : Operator.t -> string list
val string_operator_to_function_name : string -> string
val deprecated_distributions : deprecation_info String.Map.t
val deprecated_functions : deprecation_info String.Map.t
(** These functions are used by the drivers to display
    all available functions and distributions. They are
    not part of the Library interface since different drivers
    for different backends would likely want different behavior
    here *)

val pretty_print_all_math_sigs : unit Fmt.t
val pretty_print_all_math_distributions : unit Fmt.t

(** These functions related to variadic functions
    are specific to this backend and used
    during code generation *)

(* reduce_sum helpers *)
val is_reduce_sum_fn : string -> bool

(* variadic ODE helpers *)
val is_variadic_ode_fn : string -> bool
val is_variadic_ode_nonadjoint_tol_fn : string -> bool
val ode_tolerances_suffix : string
val variadic_ode_adjoint_fn : string

(* variadic DAE helpers *)
val is_variadic_dae_fn : string -> bool
val is_variadic_dae_tol_fn : string -> bool
val dae_tolerances_suffix : string
