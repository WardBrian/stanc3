open! Core_kernel
open! Core_kernel.Poly
open! Middle
open Cpp
open! Lower_expr

let unused s = Cast (Void, Var s)

let lower_st st adtype =
  lower_unsizedtype_local adtype (SizedType.to_unsized st)

let nan_type st adtype =
  match (adtype, st) with
  | UnsizedType.AutoDiffable, _ -> Var "DUMMY_VAR__"
  | DataOnly, _ -> Exprs.quiet_NaN

(*Pretty printer for the right hand side of expressions to initialize objects.
  * For scalar types this sets the value to NaN and for containers initializes the memory.
*)
let rec initialize_value st adtype =
  let open Expression_syntax in
  let init_nan = nan_type st adtype in
  if adtype = UnsizedType.DataOnly then
    match st with
    | SizedType.SInt -> Exprs.int_min
    | SReal -> init_nan
    | SComplex ->
        let scalar = local_scalar (SizedType.to_unsized st) adtype in
        Constructor (Types.complex scalar, [init_nan; init_nan])
    | SComplexVector size
     |SComplexRowVector size
     |SVector (_, size)
     |SRowVector (_, size) ->
        let typ = lower_st st adtype in
        typ |::? ("Constant", [lower_expr size; init_nan])
    | SMatrix (_, d1, d2) | SComplexMatrix (d1, d2) ->
        let typ = lower_st st adtype in
        typ |::? ("Constant", [lower_expr d1; lower_expr d2; init_nan])
    | SArray (t, d) ->
        let typ = lower_st st adtype in
        Constructor (typ, [lower_expr d; initialize_value t adtype])
  else
    let ut = SizedType.to_unsized st in
    match st with
    | SizedType.SInt -> Exprs.int_min
    | SReal -> init_nan
    | SComplex ->
        let scalar = local_scalar (SizedType.to_unsized st) adtype in
        Constructor (Types.complex scalar, [init_nan; init_nan])
    | SVector (AoS, size)
     |SRowVector (AoS, size)
     |SComplexVector size
     |SComplexRowVector size ->
        let typ = lower_st st adtype in
        typ |::? ("Constant", [lower_expr size; init_nan])
    | SMatrix (AoS, d1, d2) | SComplexMatrix (d1, d2) ->
        let typ = lower_st st adtype in
        typ |::? ("Constant", [lower_expr d1; lower_expr d2; init_nan])
    | SVector (SoA, size) ->
        let typ = lower_possibly_var_decl adtype ut SoA in
        Constructor (typ, [initialize_value (SVector (AoS, size)) DataOnly])
    | SRowVector (SoA, size) ->
        let typ = lower_possibly_var_decl adtype ut SoA in
        Constructor (typ, [initialize_value (SRowVector (AoS, size)) DataOnly])
    | SMatrix (SoA, d1, d2) ->
        let typ = lower_possibly_var_decl adtype ut SoA in
        Constructor (typ, [initialize_value (SMatrix (AoS, d1, d2)) DataOnly])
    | SArray (t, d) ->
        let typ =
          lower_possibly_var_decl adtype (SizedType.to_unsized st)
            (SizedType.get_mem_pattern t) in
        Constructor (typ, [lower_expr d; initialize_value t adtype])

(*Initialize an object of a given size.*)
let lower_assign_sized (st, adtype, initialize) =
  if initialize then Some (initialize_value st adtype) else None
