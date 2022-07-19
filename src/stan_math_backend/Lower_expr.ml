open Core_kernel
open Core_kernel.Poly
open Middle
open Cpp

let stan_namespace_qualify f =
  if String.is_suffix ~suffix:"functor__" f || String.contains f ':' then f
  else "stan::math::" ^ f

let constraint_to_string = function
  | Transformation.Ordered -> Some "ordered"
  | PositiveOrdered -> Some "positive_ordered"
  | Simplex -> Some "simplex"
  | UnitVector -> Some "unit_vector"
  | CholeskyCorr -> Some "cholesky_factor_corr"
  | CholeskyCov -> Some "cholesky_factor_cov"
  | Correlation -> Some "corr_matrix"
  | Covariance -> Some "cov_matrix"
  | Lower _ -> Some "lb"
  | Upper _ -> Some "ub"
  | LowerUpper _ -> Some "lub"
  | Offset _ | Multiplier _ | OffsetMultiplier _ -> Some "offset_multiplier"
  | Identity -> None

(* retun true if the type of the expression
   is integer, real, or complex (e.g. not a container) *)
let is_scalar e =
  match Expr.Typed.type_of e with UInt | UReal | UComplex -> true | _ -> false

let is_matrix e = Expr.Typed.type_of e = UMatrix
let is_row_vector e = Expr.Typed.type_of e = URowVector
let default_multiplier = 1
let default_offset = 0

let transform_args = function
  | Transformation.Offset offset -> [offset; Expr.Helpers.int default_multiplier]
  | Multiplier multiplier -> [Expr.Helpers.int default_offset; multiplier]
  | transform -> Transformation.fold (fun args arg -> args @ [arg]) [] transform

let promote_adtype =
  List.fold
    ~f:(fun accum expr ->
      match Expr.Typed.adlevel_of expr with
      | AutoDiffable -> AutoDiffable
      | _ -> accum )
    ~init:UnsizedType.DataOnly

let suffix_args = function
  | Fun_kind.FnRng -> ["base_rng__"]
  | FnTarget -> ["lp__"; "lp_accum__"]
  | FnPlain | FnLpdf _ -> []

let rec stantype_prim_str = function
  | UnsizedType.UInt -> Int
  | UArray t -> stantype_prim_str t
  | _ -> Double

let templates udf suffix =
  match suffix with
  | Fun_kind.FnLpdf true -> [TemplateType "propto__"]
  | FnLpdf false -> [TemplateType "false"]
  | FnTarget when udf -> [TemplateType "propto__"]
  | _ -> []

let serializer_in = Var "in__"

let rec local_scalar ut ad =
  match (ut, ad) with
  | UnsizedType.UArray t, _ -> local_scalar t ad
  | _, UnsizedType.DataOnly | UInt, AutoDiffable -> stantype_prim_str ut
  | _, AutoDiffable -> Typename "local_scalar_t__"

let rec lower_type (t : UnsizedType.t) (scalar : type_) : type_ =
  match t with
  | UInt -> Int
  | UReal -> scalar
  | UComplex -> Types.complex scalar
  | UArray t -> Vector (lower_type t scalar)
  | UVector -> Types.vector scalar
  | URowVector -> Types.row_vector scalar
  | UMatrix -> Types.matrix scalar
  | UComplexVector -> Types.vector (Types.complex scalar)
  | UComplexRowVector -> Types.row_vector (Types.complex scalar)
  | UComplexMatrix -> Types.matrix (Types.complex scalar)
  | UMathLibraryFunction | UFun _ ->
      Common.FatalError.fatal_error_msg
        [%message "Function types not implemented"]

let lower_type_eigen_expr (t : UnsizedType.t) (scalar : type_) : type_ =
  match t with
  | UInt -> Int
  | UReal | UMatrix | URowVector | UVector | UComplexVector | UComplexMatrix
   |UComplexRowVector ->
      scalar
  | UComplex -> Types.complex scalar
  | UArray t ->
      (* Expressions are not accepted for arrays of Eigen::Matrix *)
      Vector (lower_type t scalar)
  | UMathLibraryFunction | UFun _ ->
      Common.FatalError.fatal_error_msg
        [%message "Function types not implemented"]

let lower_unsizedtype_local adtype ut =
  let s = local_scalar ut adtype in
  lower_type ut s

let lower_expr_type e =
  Expr.Typed.(lower_unsizedtype_local (adlevel_of e) (type_of e))

let rec lower_possibly_var_decl adtype ut mem_pattern =
  let scalar = local_scalar ut adtype in
  let var_decl p_ut =
    if mem_pattern = Mem_pattern.SoA && adtype = UnsizedType.AutoDiffable then
      TypeTrait
        ( "stan::conditional_var_value"
        , [scalar; lower_unsizedtype_local adtype p_ut] )
    else lower_unsizedtype_local adtype p_ut in
  match ut with
  | UArray t -> Types.std_vector (lower_possibly_var_decl adtype t mem_pattern)
  | UMatrix | UVector | URowVector | UComplexRowVector | UComplexVector
   |UComplexMatrix ->
      var_decl ut
  | UReal | UInt | UComplex -> lower_unsizedtype_local adtype ut
  | x -> raise_s [%message (x : UnsizedType.t) "not implemented yet"]

let rec lower_logical_op op e1 e2 =
  let prim e = Exprs.fun_call "stan::math::primitive_value" [lower_expr e] in
  Parens (BinOp (prim e1, op, prim e2))

and vector_literal ?(column = false) scalar es =
  let open Exprs in
  let fn = if column then Types.vector else Types.row_vector in
  let constructor size =
    Constructor (fn scalar, [Literal (string_of_int size)]) in
  if List.is_empty es then constructor 0
  else
    let vector = constructor (List.length es) in
    let values = lower_exprs es in
    (vector << values).@!("finished")

and read_data ut es =
  let val_method =
    match ut with
    | UnsizedType.UArray UInt -> "vals_i"
    | UArray UReal -> "vals_r"
    | UArray UComplex -> "vals_c"
    | UInt | UReal | UComplex | UVector | URowVector | UMatrix
     |UComplexMatrix | UComplexRowVector | UComplexVector | UArray _ | UFun _
     |UMathLibraryFunction ->
        Common.FatalError.fatal_error_msg
          [%message "Can't ReadData of " (ut : UnsizedType.t)] in
  let open Exprs in
  let data_context = Var "context__" in
  data_context.@((val_method, [lower_expr (List.hd_exn es)]))

and lower_fun_app _ _ _ _ _ = failwith "todo"

and lower_user_defined_fun f suffix es =
  let extra_args =
    suffix_args suffix @ ["pstream__"] |> List.map ~f:(fun v -> Var v) in
  Exprs.templated_fun_call f (templates true suffix)
    (lower_exprs es @ extra_args)

and lower_compiler_internal ad ut f es =
  let open Exprs in
  match f with
  | Internal_fun.FnMakeArray ->
      let ut =
        match ut with
        | UnsizedType.UArray ut -> ut
        | _ ->
            Common.FatalError.fatal_error_msg
              [%message
                "Array literal must have array type but found "
                  (ut : UnsizedType.t)] in
      Exprs.std_vector_expr (lower_unsizedtype_local ad ut) (lower_exprs es)
  | FnMakeRowVec -> (
    match ut with
    | UnsizedType.URowVector ->
        let st = local_scalar ut (promote_adtype es) in
        vector_literal st es
    | UMatrix ->
        fun_call "stan::math::to_matrix"
          [ std_vector_expr
              (lower_unsizedtype_local ad URowVector)
              (lower_exprs es) ]
    | UComplexRowVector ->
        let st = local_scalar ut (promote_adtype es) in
        vector_literal st es
    | UComplexMatrix ->
        fun_call "stan::math::to_matrix"
          [ std_vector_expr
              (lower_unsizedtype_local ad UComplexRowVector)
              (lower_exprs es) ]
    | _ ->
        Common.FatalError.fatal_error_msg
          [%message
            "Unexpected type for row vector literal" (ut : UnsizedType.t)] )
  | FnReadData -> read_data ut es
  | FnReadDataSerializer ->
      serializer_in.@?(("read", [lower_unsizedtype_local AutoDiffable UReal], []))
  | FnReadParam {constrain; dims; mem_pattern} -> (
      let constrain_opt = constraint_to_string constrain in
      match constrain_opt with
      | None ->
          serializer_in.@?(( "template read"
                           , [ lower_possibly_var_decl AutoDiffable ut
                                 mem_pattern ]
                           , lower_exprs dims ))
      | Some constraint_string ->
          let constraint_args = transform_args constrain in
          let lp =
            Expr.Fixed.{pattern= Var "lp__"; meta= Expr.Typed.Meta.empty} in
          let args = constraint_args @ [lp] @ dims in
          serializer_in.@?(( "template read_constrain_" ^ constraint_string
                           , [ lower_possibly_var_decl AutoDiffable ut
                                 mem_pattern; TemplateType "jacobian__" ]
                           , lower_exprs args )) )
  | FnDeepCopy ->
      lower_fun_app Fun_kind.FnPlain "stan::model::deep_copy" es Mem_pattern.AoS
        (Some UnsizedType.Void)
  | _ ->
      lower_fun_app FnPlain (Internal_fun.to_string f) es Mem_pattern.AoS
        (Some UnsizedType.Void)

and lower_expr (Expr.Fixed.{pattern; meta} : Expr.Typed.t) : Cpp.expr =
  let open Exprs in
  match pattern with
  | Var s -> Var s
  | Lit (Str, s) -> literal_string (Cpp_str.escaped s)
  | Lit (Imaginary, s) ->
      fun_call "stan::math::to_complex" [Literal "0"; Literal s]
  | Lit ((Real | Int), s) -> Literal s
  | Promotion (expr, UReal, _) when is_scalar expr -> lower_expr expr
  | Promotion (expr, UComplex, DataOnly) when is_scalar expr ->
      (* this is in principle a little better than promote_scalar since it is constexpr *)
      fun_call "stan::math::to_complex" [lower_expr expr; Literal "0"]
  | Promotion (expr, ut, ad) ->
      templated_fun_call "stan::math::promote_scalar"
        [lower_unsizedtype_local ad ut]
        [lower_expr expr]
  | EAnd (e1, e2) -> lower_logical_op And e1 e2
  | EOr (e1, e2) -> lower_logical_op Or e1 e2
  | Indexed (e, []) -> lower_expr e
  | TernaryIf (ec, et, ef) ->
      let maybe_eval e =
        if UnsizedType.is_eigen_type meta.type_ then
          fun_call "stan::math::eval" [lower_expr e]
        else lower_expr e in
      TernaryIf (maybe_eval ec, maybe_eval et, maybe_eval ef)
  | FunApp
      ( StanLib (op, _, _)
      , [ { meta= {type_= URowVector; _}
          ; pattern= FunApp (CompilerInternal FnMakeRowVec, es) } ] )
    when Operator.(Some Transpose = of_string_opt op) ->
      let st = local_scalar UVector (promote_adtype es) in
      vector_literal ~column:true st es
  | FunApp
      ( StanLib (op, _, _)
      , [ { meta= {type_= UComplexRowVector; _}
          ; pattern= FunApp (CompilerInternal FnMakeRowVec, es) } ] )
    when Operator.(Some Transpose = of_string_opt op) ->
      let st = Types.complex (local_scalar UComplexVector (promote_adtype es)) in
      vector_literal ~column:true st es
  | FunApp (CompilerInternal f, es) ->
      lower_compiler_internal meta.adlevel meta.type_ f es
  | FunApp (StanLib (f, suffix, mem_pattern), es) ->
      let ret_type = Some (UnsizedType.ReturnType meta.type_) in
      lower_fun_app suffix f es mem_pattern ret_type
  | FunApp (UserDefined (f, suffix), es) -> lower_user_defined_fun f suffix es
  | Indexed (e, idx) -> (
    match e.pattern with
    | FunApp (CompilerInternal (FnReadParam _), _) -> lower_expr e
    | _ ->
        Caml.ignore idx ;
        failwith "todo"
        (* | FunApp (CompilerInternal FnReadData, _) ->
               pp_indexed_simple ppf (str "%a" pp_expr e, idx)
           | _
             when List.for_all ~f:dont_need_range_check idx
                  && not (UnsizedType.is_indexing_matrix (Expr.Typed.type_of e, idx))
             ->
               pp_indexed_simple ppf (str "%a" pp_expr e, idx)
           | _ -> pp_indexed ppf (str "%a" pp_expr e, idx, pretty_print e) *) )

and lower_exprs = List.map ~f:lower_expr
