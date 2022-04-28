(** This module stores a table of all signatures from the Stan
    math C++ library which are exposed to Stan, and some helper
    functions for dealing with those signatures.
*)

include Frontend.Std_library_utils.Library

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
