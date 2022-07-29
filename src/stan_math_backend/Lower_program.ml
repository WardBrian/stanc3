open Core_kernel
open Core_kernel.Poly
open Middle
open! Lower_expr
open! Lower_stmt
open Lower_functions
open Cpp

let standalone_functions = ref false
let stanc_args_to_print = ref ""

let get_unconstrained_param_st lst =
  match lst with
  | _, {Program.out_block= Parameters; out_unconstrained_st= st; _} -> (
    match SizedType.get_dims_io st with
    | [] -> Some [Expr.Helpers.loop_bottom]
    | ls -> Some ls )
  | _ -> None

let get_constrained_param_st lst =
  match lst with
  | _, {Program.out_block= Parameters; out_constrained_st= st; _} -> (
    match SizedType.get_dims_io st with
    | [] -> Some [Expr.Helpers.loop_bottom]
    | ls -> Some ls )
  | _ -> None

let lower_num_param (dims : Expr.Typed.t list) =
  match dims with
  | [] -> Literal "0"
  | [d] -> lower_expr d
  | d1 :: dims ->
      Parens
        (List.fold ~init:(lower_expr d1) ~f:Expression_syntax.( * )
           (lower_exprs dims) )

(** Create a variable for the name of model function.
  @param prog_name Name of the Stan program.
  @param fname Name of the function.
 *)
let gen_function__ prog_name fname =
  [ VarDef
      (make_var_defn ~static:true ~constexpr:true
         ~type_:(Const (Pointer (Type_literal "char"))) ~name:"function__"
         ~init:
           (Assignment
              (Exprs.literal_string (prog_name ^ "_namespace::" ^ fname)) )
         () ) ]
  @ Stmts.unused "function__"

let version = TopComment "Code generated by %%NAME%% %%VERSION%%"
let includes = Preprocessor (Include "stan/model/model_header.hpp")

(** Print the private members of the model class

  Accounting for types that can be moved to OpenCL.
  @param ppf A formatter
  @param vident name of the private member.
  @param ut The unsized type to print.
 *)
let lower_data_decl (vident, ut) : defn =
  let data_vident =
    match Transform_Mir.is_opencl_var vident with
    | false when UnsizedType.is_eigen_type ut -> vident ^ "_data__"
    | _ -> vident in
  TopVarDef (lower_unsized_decl data_vident ut DataOnly)

(** Create maps of Eigen types*)
let lower_map_decl (vident, ut) : defn =
  let eigen_map t ndims =
    TopVarDef
      (make_var_defn
         ~type_:(TypeTrait ("Eigen::Map", [t]))
         ~name:vident
         ~init:
           (InitalizerList
              (Literal "nullptr" :: List.init ndims ~f:(fun _ -> Literal "0"))
           )
         () ) in
  let scalar = local_scalar ut DataOnly in
  let open Types in
  match ut with
  | UMatrix -> eigen_map (matrix scalar) 2
  | URowVector -> eigen_map (row_vector scalar) 1
  | UVector -> eigen_map (vector scalar) 1
  | UComplexMatrix -> eigen_map (matrix (complex scalar)) 2
  | UComplexRowVector -> eigen_map (row_vector (complex scalar)) 1
  | UComplexVector -> eigen_map (vector (complex scalar)) 1
  | x ->
      Common.FatalError.fatal_error_msg
        [%message
          "Error during Map data construction for " vident " of type "
            (x : UnsizedType.t)]

let rec top_level_decls Stmt.Fixed.{pattern; _} =
  match pattern with
  | Decl d when d.decl_id <> "pos__" ->
      [(d.decl_id, Type.to_unsized d.decl_type)]
  | SList stmts -> List.concat_map ~f:top_level_decls stmts
  | _ -> []

(** Generate the private data members of the model class *)
let lower_model_private {Program.prepare_data; _} =
  let data_decls = List.concat_map ~f:top_level_decls prepare_data in
  (*Filter out Any data that is not an Eigen matrix*)
  let get_eigen_map (name, ut) =
    if UnsizedType.is_eigen_type ut && not (Transform_Mir.is_opencl_var name)
    then true
    else false in
  let eigen_map_decls = (List.filter ~f:get_eigen_map) data_decls in
  List.map ~f:lower_data_decl data_decls
  @ List.map ~f:lower_map_decl eigen_map_decls

let gen_validate_data name st =
  if String.is_suffix ~suffix:"__" name then []
  else
    let vector args =
      let cast x =
        Exprs.templated_fun_call "static_cast" [Types.size_t] [lower_expr x]
      in
      InitializerExpr (Types.std_vector Types.size_t, List.map ~f:cast args)
    in
    let open Expression_syntax in
    let context = Var "context__" in
    let validate =
      context.@?(( "validate_dims"
                 , [ literal_string "data initialization"
                   ; literal_string (Mangle.remove_prefix name)
                   ; literal_string
                       (Fmt.to_to_string Cpp.Printing.pp_type_
                          (stantype_prim (SizedType.to_unsized st)) )
                   ; vector (SizedType.get_dims_io st) ] )) in
    [Expression validate]

let gen_assign_data decl_id st =
  let lower_placement_new decl_id st =
    let open Expression_syntax in
    match st with
    | SizedType.SVector (_, d)
     |SRowVector (_, d)
     |SComplexVector d
     |SComplexRowVector d ->
        let data = Var (decl_id ^ "_data__") in
        [ Expression
            (New
               ( Some ("&" ^ decl_id)
               , TypeTrait ("Eigen::Map", [lower_st st DataOnly])
               , [data.@!("data"); lower_expr d] ) ) ]
    | SMatrix (_, d1, d2) | SComplexMatrix (d1, d2) ->
        let data = Var (decl_id ^ "_data__") in
        [ Expression
            (New
               ( Some ("&" ^ decl_id)
               , TypeTrait ("Eigen::Map", [lower_st st DataOnly])
               , [data.@!("data"); lower_expr d1; lower_expr d2] ) ) ]
    | _ -> [] in
  let underlying_var decl_id st =
    match st with
    | SizedType.SVector _ | SRowVector _ | SMatrix _ | SComplexVector _
     |SComplexRowVector _ | SComplexMatrix _ ->
        Var (decl_id ^ "_data__")
    | SInt | SReal | SComplex | SArray _ -> Var decl_id in
  Expression (Assign (underlying_var decl_id st, initialize_value st DataOnly))
  :: lower_placement_new decl_id st

let lower_constructor
    {Program.prog_name; input_vars; prepare_data; output_vars; _} =
  let args =
    [ (Ref (Type_literal "stan::io::var_context"), "context__")
    ; (Type_literal "unsigned int", "random_seed__ = 0")
    ; (Pointer (Type_literal "std::ostream"), "pstream__ = nullptr") ] in
  let preamble =
    [ Decls.current_statement; Using ("local_scalar_t__", Some Double)
    ; VarDef
        (make_var_defn ~type_:(Type_literal "boost::ecuyer1988")
           ~name:"base_rng__"
           ~init:
             (Assignment
                (Exprs.fun_call "stan::services::util::create_rng"
                   [Var "random_seed__"; Literal "0"] ) )
           () ) ]
    @ Stmts.unused "base_rng__"
    @ gen_function__ prog_name prog_name
    @ Decls.dummy_var in
  let data_idents = List.map ~f:fst input_vars |> String.Set.of_list in
  let lower_data (Stmt.Fixed.{pattern; meta} as s) =
    match pattern with
    | Decl {decl_id; decl_type; _} when decl_id <> "pos__" -> (
      match decl_type with
      | Sized st -> (
          Locations.create_loc_assignment meta
          @
          match Set.mem data_idents decl_id with
          | true -> gen_validate_data decl_id st @ gen_assign_data decl_id st
          | false -> gen_assign_data decl_id st )
      | Unsized _ -> [] )
    | _ -> lower_statement s in
  let data =
    [Stmts.rethrow_located (List.concat_map ~f:lower_data prepare_data)] in
  let set_num_params =
    let output_params =
      List.filter_map ~f:get_unconstrained_param_st output_vars in
    let add_params par pars =
      List.fold ~init:par ~f:Expression_syntax.( + ) pars in
    match output_params with
    | [] -> Expression (Assign (Var "num_params_r__", Literal "0U"))
    | [par] -> Expression (Assign (Var "num_params_r__", lower_num_param par))
    | par :: pars ->
        Expression
          (Assign
             ( Var "num_params_r__"
             , add_params (lower_num_param par)
                 (List.map ~f:lower_num_param pars) ) ) in
  make_constructor ~args
    ~init_list:[("model_base_crtp", [Literal "0"])]
    ~body:(preamble @ data @ [set_num_params])
    ()

let gen_log_prob Program.{prog_name; log_prob; _} =
  let templates =
    [ Bool "propto__"; Bool "jacobian__"; Typename "VecR"; Typename "VecI"
    ; Require ("stan::require_vector_like_t", ["VecR"])
    ; Require ("stan::require_vector_like_vt", ["std::is_integral"; "VecI"]) ]
  in
  let args : (type_ * string) list =
    [ (Ref (TemplateType "VecR"), "params_r__")
    ; (Ref (TemplateType "VecI"), "params_i__")
    ; (Pointer (Type_literal "std::ostream"), "pstream__ = nullptr") ] in
  let intro =
    let t__ = Type_literal "T__" in
    [ Using
        ("T__", Some (TypeTrait ("stan::scalar_type_t", [TemplateType "VecR"])))
    ; Using ("local_scalar_t__", Some t__)
    ; VarDef
        (make_var_defn ~type_:t__ ~name:"lp__"
           ~init:(Construction [Literal "0.0"])
           () ); Decls.lp_accum t__; Decls.serializer_in
    ; Decls.current_statement ]
    @ Decls.dummy_var
    @ gen_function__ prog_name "log_prob" in
  let outro =
    let open Expression_syntax in
    let lp_accum__ = Var "lp_accum__" in
    [ Expression lp_accum__.@?(("add", [Var "lp__"]))
    ; Return (Some lp_accum__.@!("sum")) ] in
  FunDef
    (make_fun_defn
       ~templates_init:([templates], true)
       ~inline:true
       ~return_type:(TypeTrait ("stan::scalar_type_t", [TemplateType "VecR"]))
       ~name:"log_prob_impl" ~args
       ~body:
         (intro @ (Stmts.rethrow_located (lower_statements log_prob) :: outro))
       ~cv_qualifiers:[Const] () )

let gen_write_array {Program.prog_name; generate_quantities; _} =
  let templates =
    [ Typename "RNG"; Typename "VecR"; Typename "VecI"; Typename "VecVar"
    ; Require
        ("stan::require_vector_like_vt", ["std::is_floating_point"; "VecR"])
    ; Require ("stan::require_vector_like_vt", ["std::is_integral"; "VecI"])
    ; Require ("stan::require_vector_vt", ["std::is_floating_point"; "VecVar"])
    ] in
  let args =
    [ (Ref (TemplateType "RNG"), "base_rng__")
    ; (Ref (TemplateType "VecR"), "params_r__")
    ; (Ref (TemplateType "VecI"), "params_i__")
    ; (Ref (TemplateType "VecVar"), "vars__")
    ; (Const Types.bool, "emit_transformed_parameters__ = true")
    ; (Const Types.bool, "emit_generated_quantities__ = true")
    ; (Pointer (Type_literal "std::ostream"), "pstream__ = nullptr") ] in
  let intro =
    [ Using ("local_scalar_t__", Some Double); Decls.serializer_in
    ; Decls.serializer_out
    ; VarDef
        (make_var_defn ~static:true ~constexpr:true ~type_:Types.bool
           ~name:"propto__" ~init:(Assignment (Literal "true")) () ) ]
    @ Stmts.unused "propto__"
    @ VarDef
        (make_var_defn ~type_:Double ~name:"lp__"
           ~init:(Assignment (Literal "0.0")) () )
      :: Stmts.unused "lp__"
    @ [Decls.current_statement; Decls.lp_accum Double]
    @ Decls.dummy_var
    @ [ VarDef
          (make_var_defn ~constexpr:true ~type_:Types.bool ~name:"jacobian__"
             ~init:(Assignment (Literal "false")) () ) ]
    @ gen_function__ prog_name "write_array" in
  FunDef
    (make_fun_defn
       ~templates_init:([templates], true)
       ~inline:true ~return_type:Void ~name:"write_array_impl" ~args
       ~body:
         (intro @ [Stmts.rethrow_located (lower_statements generate_quantities)])
       ~cv_qualifiers:[Const] () )

let gen_transform_inits_impl {Program.transform_inits; _} =
  let templates =
    [ Typename "VecVar"; Typename "VecI"
    ; Require ("stan::require_vector_t", ["VecVar"])
    ; Require ("stan::require_vector_like_vt", ["std::is_integral"; "VecI"]) ]
  in
  let args =
    [ (Ref (TemplateType "VecVar"), "params_r__")
    ; (Ref (TemplateType "VecI"), "params_i__")
    ; (Ref (TemplateType "VecVar"), "vars__")
    ; (Pointer (Type_literal "std::ostream"), "pstream__ = nullptr") ] in
  let intro =
    [ Using ("local_scalar_t__", Some Double); Decls.serializer_in
    ; Decls.serializer_out; Decls.current_statement ]
    @ Decls.dummy_var in
  FunDef
    (make_fun_defn
       ~templates_init:([templates], true)
       ~inline:true ~return_type:Void ~name:"transform_inits_impl" ~args
       ~body:(intro @ [Stmts.rethrow_located (lower_statements transform_inits)])
       ~cv_qualifiers:[Const] () )

let gen_get_param_names {Program.output_vars; _} =
  let extract_name var = Exprs.literal_string (Mangle.remove_prefix (fst var)) in
  let args = [(Ref (Types.std_vector Types.string), "names__")] in
  let body =
    [ Expression
        (Assign
           ( Var "names__"
           , Exprs.std_vector_expr Types.string
               (List.map ~f:extract_name output_vars) ) ) ] in
  FunDef
    (make_fun_defn ~inline:true ~return_type:Void ~name:"get_param_names" ~args
       ~body ~cv_qualifiers:[Const] () )

let gen_get_dims {Program.output_vars; _} =
  let cast x =
    Exprs.templated_fun_call "static_cast" [Types.size_t] [lower_expr x] in
  let pack inner_dims =
    Exprs.std_vector_expr Types.size_t
      (List.map ~f:cast (SizedType.get_dims_io inner_dims)) in
  let dim_list =
    List.(
      map ~f:(fun (_, {Program.out_constrained_st= st; _}) -> st) output_vars)
  in
  let result_vector =
    Exprs.std_vector_expr
      (Types.std_vector Types.size_t)
      (List.map ~f:pack dim_list) in
  FunDef
    (make_fun_defn ~inline:true ~return_type:Void ~name:"get_dims"
       ~args:
         [(Ref (Types.std_vector (Types.std_vector Types.size_t)), "dimss__")]
       ~body:[Expression (Assign (Var "dimss__", result_vector))]
       ~cv_qualifiers:[Const] () )

let emplace_name name idcs =
  let name = Mangle.remove_prefix name in
  let to_string e = Exprs.fun_call "std::to_string" [Var e] in
  let param_names__ = Var "param_names__" in
  let null_string = Constructor (Types.string, []) in
  let dot = Literal "'.'" in
  let open Expression_syntax in
  [ Expression
      param_names__.@?(( "emplace_back"
                       , [ null_string
                           + List.fold ~init:(literal_string name)
                               ~f:(fun acc idx -> acc + dot + to_string idx)
                               idcs ] )) ]

let emplace_complex_name name idcs =
  let name = Mangle.remove_prefix name in
  let to_string e = Exprs.fun_call "std::to_string" [Var e] in
  let param_names__ = Var "param_names__" in
  let null_string = Constructor (Types.string, []) in
  let dot = Literal "'.'" in
  let open Expression_syntax in
  [ Expression
      param_names__.@?(( "emplace_back"
                       , [ null_string + literal_string name
                           + List.fold_left ~init:(literal_string "real")
                               ~f:(fun acc idx -> to_string idx + dot + acc)
                               idcs ] ))
  ; Expression
      param_names__.@?(( "emplace_back"
                       , [ null_string + literal_string name
                           + List.fold_left ~init:(literal_string "imag")
                               ~f:(fun acc idx -> to_string idx + dot + acc)
                               idcs ] )) ]

let rec gen_indexing_loop ?(index_ids = []) iteratee dims gen_body =
  let iter d gen_body =
    let loopvar, gensym_exit = Common.Gensym.enter () in
    let forloop =
      Stmts.fori loopvar
        (lower_expr Expr.Helpers.loop_bottom)
        d
        (Stmts.block @@ gen_body iteratee (loopvar :: index_ids)) in
    gensym_exit () ; forloop in
  match dims with
  | [] -> gen_body iteratee index_ids
  | dim :: dims ->
      [ iter dim (fun i idcs ->
            gen_indexing_loop ~index_ids:idcs i dims gen_body ) ]

let gen_param_names_fn name (paramvars, tparamvars, gqvars) =
  let args =
    [ (Ref (Types.std_vector Types.string), "param_names__")
    ; (Types.bool, "emit_transformed_parameters__ = true")
    ; (Types.bool, "emit_generated_quantities__ = true") ] in
  let gen_param_names (decl_id, st) =
    let gen_name =
      if SizedType.contains_complex st then emplace_complex_name
      else emplace_name in
    let dims = lower_exprs (List.rev (SizedType.get_dims st)) in
    gen_indexing_loop decl_id dims gen_name in
  let body =
    List.concat_map ~f:gen_param_names paramvars
    @ [ IfElse
          ( Var "emit_transformed_parameters__"
          , Stmts.block (List.concat_map ~f:gen_param_names tparamvars)
          , None )
      ; IfElse
          ( Var "emit_generated_quantities__"
          , Stmts.block (List.concat_map ~f:gen_param_names gqvars)
          , None ) ] in
  FunDef
    (make_fun_defn ~inline:true ~return_type:Void ~name ~args ~body
       ~cv_qualifiers:[Const; Final] () )

let gen_constrained_param_names {Program.output_vars; _} =
  gen_param_names_fn "constrained_param_names"
    (List.partition3_map
       ~f:(function
         | id, {Program.out_block= Parameters; out_constrained_st= st; _} ->
             `Fst (id, st)
         | id, {out_block= TransformedParameters; out_constrained_st= st; _} ->
             `Snd (id, st)
         | id, {out_block= GeneratedQuantities; out_constrained_st= st; _} ->
             `Trd (id, st) )
       output_vars )

let gen_unconstrained_param_names {Program.output_vars; _} =
  gen_param_names_fn "unconstrained_param_names"
    (List.partition3_map
       ~f:(function
         | id, {Program.out_block= Parameters; out_unconstrained_st= st; _} ->
             `Fst (id, st)
         | id, {out_block= TransformedParameters; out_unconstrained_st= st; _}
           ->
             `Snd (id, st)
         | id, {out_block= GeneratedQuantities; out_unconstrained_st= st; _} ->
             `Trd (id, st) )
       output_vars )

(** Create constrained and unconstrained sizedtype methods
 in the model class
 @param name The name of the method to wrap the body in.
 @param outvars The parameters to gather the sizes for.
 *)
let gen_outvar_metadata name outvars =
  let json_str = Cpp_Json.out_var_interpolated_json_str outvars in
  FunDef
    (make_fun_defn ~inline:true ~return_type:Types.string ~name
       ~body:
         [ Return
             (Some (Constructor (Types.string, [Exprs.literal_string json_str])))
         ]
       ~cv_qualifiers:[Const] () )

(** Print the [get_unconstrained_sizedtypes] method of the model class *)
let gen_unconstrained_types {Program.output_vars; _} =
  let grab_unconstrained (name, {Program.out_unconstrained_st; out_block; _}) =
    (name, out_unconstrained_st, out_block) in
  let outvars = List.map ~f:grab_unconstrained output_vars in
  gen_outvar_metadata "get_unconstrained_sizedtypes" outvars

(** Print the [get_constrained_sizedtypes] method of the model class *)
let gen_constrained_types {Program.output_vars; _} =
  let grab_constrained (name, {Program.out_constrained_st; out_block; _}) =
    (name, out_constrained_st, out_block) in
  let outvars = List.map ~f:grab_constrained output_vars in
  gen_outvar_metadata "get_constrained_sizedtypes" outvars

(** The generic method overloads needed in the model class. *)
let gen_overloads {Program.output_vars; _} =
  let pstream = (Pointer (Type_literal "std::ostream"), "pstream = nullptr") in
  let write_arrays =
    let open Expression_syntax in
    let templates_init = ([[Typename "RNG"]], false) in
    let emit_flags const =
      let t : type_ = if const then Const Types.bool else Types.bool in
      [ (t, "emit_transformed_parameters = true")
      ; (t, "emit_generated_quantities = true") ] in
    let sizes =
      (* An expression for the number of individual parameters in a list of output variables *)
      let num_outvars (outvars : Expr.Typed.t Program.outvar list) =
        Expr.Helpers.binop_list
          (List.map
             ~f:(fun outvar ->
               SizedType.num_elems_expr outvar.Program.out_constrained_st )
             outvars )
          Operator.Plus ~default:Expr.Helpers.zero
        |> lower_expr in
      (* The list of output variables that came from a particular block *)
      let block_outvars (block : Program.io_block) =
        List.filter_map output_vars
          ~f:(fun ((_ : string), (outvar : Expr.Typed.t Program.outvar)) ->
            if outvar.out_block = block then Some outvar else None ) in
      let num_params = num_outvars (block_outvars Parameters) in
      let num_transformed = num_outvars (block_outvars TransformedParameters) in
      let num_gen_quantities = num_outvars (block_outvars GeneratedQuantities) in
      [ VarDef
          (make_var_defn ~type_:(Const Types.size_t) ~name:"num_params__"
             ~init:(Assignment num_params) () )
      ; VarDef
          (make_var_defn ~type_:(Const Types.size_t) ~name:"num_transformed"
             ~init:
               (Assignment
                  (Var "emit_transformed_parameters" * Parens num_transformed)
               )
             () )
      ; VarDef
          (make_var_defn ~type_:(Const Types.size_t) ~name:"num_gen_quantities"
             ~init:
               (Assignment
                  (Var "emit_generated_quantities" * Parens num_gen_quantities)
               )
             () )
      ; VarDef
          (make_var_defn ~type_:(Const Types.size_t) ~name:"num_to_write"
             ~init:
               (Assignment
                  ( Var "num_params__" + Var "num_transformed"
                  + Var "num_gen_quantities" ) )
             () ) ] in
    let call_impl =
      Expression
        (Exprs.fun_call "write_array_impl"
           [ Var "base_rng"; Var "params_r"; Var "params_i"; Var "vars"
           ; Var "emit_transformed_parameters"; Var "emit_generated_quantities"
           ; Var "pstream" ] ) in
    [ FunDef
        (make_fun_defn ~templates_init ~inline:true ~return_type:Void
           ~name:"write_array"
           ~args:
             ( [ (Ref (TemplateType "RNG"), "base_rng")
               ; (Ref (Types.vector Double), "params_r")
               ; (Ref (Types.vector Double), "vars") ]
             @ emit_flags true @ [pstream] )
           ~body:
             ( sizes
             @ [ VarDef
                   (make_var_defn ~type_:(Types.std_vector Int) ~name:"params_i"
                      () )
               ; Expression
                   (Assign
                      ( Var "vars"
                      , Types.vector Double
                        |::? ("Constant", [Var "num_to_write"; Exprs.quiet_NaN])
                      ) ); call_impl ] )
           ~cv_qualifiers:[Const] () )
    ; FunDef
        (make_fun_defn ~templates_init ~inline:true ~return_type:Void
           ~name:"write_array"
           ~args:
             ( [ (Ref (TemplateType "RNG"), "base_rng")
               ; (Ref (Types.std_vector Double), "params_r")
               ; (Ref (Types.std_vector Int), "params_i")
               ; (Ref (Types.std_vector Double), "vars") ]
             @ emit_flags false @ [pstream] )
           ~body:
             ( sizes
             @ [ Expression
                   (Assign
                      ( Var "vars"
                      , Constructor
                          ( Types.std_vector Double
                          , [Var "num_to_write"; Exprs.quiet_NaN] ) ) )
               ; call_impl ] )
           ~cv_qualifiers:[Const] () ) ] in
  let log_probs =
    let templates_init =
      ([[Bool "propto__"; Bool "jacobian__"; Typename "T_"]], false) in
    let call_impl =
      Return
        (Some
           (Exprs.templated_fun_call "log_prob_impl"
              [TemplateType "propto__"; TemplateType "jacobian__"]
              [Var "params_r"; Var "params_i"; Var "pstream"] ) ) in
    [ FunDef
        (make_fun_defn ~templates_init ~inline:true
           ~return_type:(TemplateType "T_") ~name:"log_prob"
           ~args:[(Ref (Types.vector (TemplateType "T_")), "params_r"); pstream]
           ~body:
             [ VarDef
                 (make_var_defn ~type_:(Types.vector Int) ~name:"params_i" ())
             ; call_impl ]
           ~cv_qualifiers:[Const] () )
    ; FunDef
        (make_fun_defn ~templates_init ~inline:true
           ~return_type:(TemplateType "T_") ~name:"log_prob"
           ~args:
             [ (Ref (Types.std_vector (TemplateType "T_")), "params_r")
             ; (Ref (Types.std_vector Int), "params_i"); pstream ]
           ~body:[call_impl] ~cv_qualifiers:[Const] () ) ] in
  let transform_inits =
    [ FunDef
        (make_fun_defn ~inline:true ~return_type:Void ~name:"transform_inits"
           ~args:
             [ ( Types.const_ref (Type_literal "stan::io::var_context")
               , "context" ); (Ref (Types.vector Double), "params_r"); pstream
             ]
           ~body:
             [ VarDef
                 (make_var_defn ~type_:(Types.std_vector Double)
                    ~name:"params_r_vec"
                    ~init:
                      (Construction
                         [Exprs.method_call (Var "params_r") "size" [] []] )
                    () )
             ; VarDef
                 (make_var_defn ~type_:(Types.std_vector Int) ~name:"params_i"
                    () )
             ; Expression
                 (Exprs.fun_call "transform_inits"
                    [ Var "context"; Var "params_i"; Var "params_r_vec"
                    ; Var "pstream" ] )
             ; Expression
                 (Assign
                    ( Var "params_r"
                    , Constructor
                        ( TypeTrait ("Eigen::Map", [Types.vector Double])
                        , let params_r_vec = Var "params_r_vec" in
                          let open Expression_syntax in
                          [params_r_vec.@!("data"); params_r_vec.@!("size")] )
                    ) ) ]
           ~cv_qualifiers:[Const; Final] () ) ] in
  (TopComment "Begin method overload boilerplate" :: write_arrays)
  @ log_probs @ transform_inits

let gen_transform_inits {Program.output_vars; _} =
  let args =
    [ (Types.const_ref (Type_literal "stan::io::var_context"), "context")
    ; (Ref (Types.std_vector Int), "params_i")
    ; (Ref (Types.std_vector Double), "vars")
    ; (Pointer (Type_literal "std::ostream"), "pstream__ = nullptr") ] in
  let list_names ((stri : string), (Program.{out_block; _} : 'a Program.outvar))
      =
    match out_block with Parameters -> Some stri | _ -> None in
  let param_names = List.filter_map ~f:list_names output_vars in
  let list_len = List.length param_names in
  let constrained_params =
    List.filter_map ~f:get_constrained_param_st output_vars in
  let names_array =
    VarDef
      (make_var_defn ~constexpr:true ~type_:(Types.str_array list_len)
         ~name:"names__"
         ~init:
           (InitalizerList
              (List.map
                 ~f:(Fn.compose Exprs.literal_string Mangle.remove_prefix)
                 param_names ) )
         () ) in
  let constrained_params_arr =
    VarDef
      (make_var_defn
         ~type_:(Const (Array (Type_literal "Eigen::Index", list_len)))
         ~name:"constrain_param_sizes__"
         ~init:(InitalizerList (List.map ~f:lower_num_param constrained_params))
         () ) in
  let num_constrained_param_size =
    let constrain_param_sizes = Var "constrain_param_sizes__" in
    let open Expression_syntax in
    VarDef
      (make_var_defn ~type_:(Const Auto) ~name:"num_constrained_params__"
         ~init:
           (Assignment
              (Exprs.fun_call "std::accumulate"
                 [ constrain_param_sizes.@!("begin")
                 ; constrain_param_sizes.@!("end"); Literal "0" ] ) )
         () ) in
  let flatten_and_call =
    let open Expression_syntax in
    [ VarDef
        (make_var_defn ~type_:(Types.std_vector Double) ~name:"params_r_flat__"
           ~init:(Construction [Var "num_constrained_params__"])
           () )
    ; VarDef
        (make_var_defn ~type_:(Type_literal "Eigen::Index") ~name:"size_iter__"
           ~init:(Assignment (Literal "0")) () )
    ; VarDef
        (make_var_defn ~type_:(Type_literal "Eigen::Index") ~name:"flat_iter__"
           ~init:(Assignment (Literal "0")) () )
    ; ForEach
        ( (Auto, "&&param_name__")
        , Var "names__"
        , Block
            [ VarDef
                (make_var_defn ~type_:(Const Auto) ~name:"param_vec__"
                   ~init:
                     (Assignment
                        (Var "context").@?(("vals_r", [Var "param_name__"])) )
                   () )
            ; For
                ( make_var_defn ~type_:(Type_literal "Eigen::Index") ~name:"i"
                    ~init:(Assignment (Literal "0")) ()
                , BinOp
                    ( Var "i"
                    , Lthn
                    , Index (Var "constrain_param_sizes__", Var "size_iter__")
                    )
                , Unary (Incr, Var "i")
                , Block
                    [ Expression
                        (Assign
                           ( Index (Var "params_r_flat__", Var "flat_iter__")
                           , Index (Var "param_vec__", Var "i") ) )
                    ; Expression (Unary (Incr, Var "flat_iter__")) ] )
            ; Expression (Unary (Incr, Var "size_iter__")) ] )
    ; Expression (Var "vars").@?(("resize", [Var "num_params_r__"]))
    ; Expression
        (Exprs.fun_call "transform_inits_impl"
           [Var "params_r_flat__"; Var "params_i"; Var "vars"; Var "pstream__"] )
    ] in
  FunDef
    (make_fun_defn ~inline:true ~return_type:Void ~name:"transform_inits" ~args
       ~body:
         ( [names_array; constrained_params_arr; num_constrained_param_size]
         @ flatten_and_call )
       ~cv_qualifiers:[Const] () )

let lower_model_public p =
  [ gen_log_prob p; gen_write_array p; gen_transform_inits_impl p
  ; (* Begin metadata methods *) gen_get_param_names p
  ; (* Post-data metadata methods *) gen_get_dims p
  ; gen_constrained_param_names p; gen_unconstrained_param_names p
  ; gen_constrained_types p; gen_unconstrained_types p ]
  (* Boilerplate *)
  @ gen_overloads p
  @ [gen_transform_inits p]

let model_public_basics name =
  [ FunDef
      (make_fun_defn ~inline:true ~return_type:Types.string ~name:"model_name"
         ~cv_qualifiers:[Const; Final]
         ~body:[Return (Some (Exprs.literal_string name))]
         () )
  ; FunDef
      (make_fun_defn ~inline:true
         ~return_type:(Types.std_vector Types.string)
         ~name:"model_compile_info"
         ~body:
           [ Return
               (Some
                  (Exprs.std_vector_expr Types.string
                     [ Exprs.literal_string
                         "stanc_version = %%NAME%%3 %%VERSION%%"
                     ; Exprs.literal_string
                         ("stancflags = " ^ !stanc_args_to_print) ] ) ) ]
         ~cv_qualifiers:[Const; NoExcept] () ) ]

let lower_model ({Program.prog_name; _} as p) =
  let private_members = lower_model_private p in
  let public_members = model_public_basics prog_name @ lower_model_public p in
  let constructor = lower_constructor p in
  Class
    (make_class_defn ~name:prog_name ~final:true
       ~base:(Type_literal ("public model_base_crtp<" ^ prog_name ^ ">"))
       ~private_members ~public_members ~constructor () )

(** Create the model's namespace. *)
let namespace Program.{prog_name; _} = prog_name ^ "_namespace"

(** Register functiors used for map_rect.
    Assumes hashtable map_rect_calls is populated *)
let register_map_rect_functors namespace =
  let register_functor (i, f) =
    Preprocessor
      (MacroApply
         ("STAN_REGISTER_MAP_RECT", [string_of_int i; namespace ^ "::" ^ f]) )
  in
  List.map ~f:register_functor
    (List.sort ~compare (Hashtbl.to_alist map_rect_calls))

let usings =
  [ TopUsing ("stan::model::model_base_crtp", None)
  ; TopUsing ("namespace stan::math", None) ]

(** Model boilerplate. *)
let new_model_boilerplate prog_name =
  let new_model =
    let args =
      [ (Ref (Type_literal "stan::io::var_context"), "data_context")
      ; (Type_literal "unsigned int", "seed")
      ; (Pointer (Type_literal "std::ostream"), "msg_stream") ] in
    let body =
      [ VarDef
          (make_var_defn ~type_:(Pointer (Type_literal "stan_model")) ~name:"m"
             ~init:
               (Assignment
                  (New
                     ( None
                     , Type_literal "stan_model"
                     , [Var "data_context"; Var "seed"; Var "msg_stream"] ) ) )
             () ); Return (Some (Literal "*m")) ] in
    FunDef
      (make_fun_defn ~name:"new_model"
         ~return_type:(Ref (Type_literal "stan::model::model_base")) ~args ~body
         () ) in
  let profile_data =
    FunDef
      (make_fun_defn ~name:"get_stan_profile_data"
         ~return_type:(Ref (Type_literal "stan::math::profile_map"))
         ~body:[Return (Some (Literal (prog_name ^ "_namespace::profiles__")))]
         () ) in
  [ TopUsing
      ( "stan_model"
      , Some (Type_literal (prog_name ^ "_namespace::" ^ prog_name)) )
  ; Preprocessor
      (IfNDef ("USING_R", [TopComment "Boilerplate"; new_model; profile_data]))
  ]

let lower_program (p : Program.Typed.t) : Cpp.program =
  Hashtbl.clear Lower_expr.map_rect_calls ;
  (* First, do some transformations on the MIR itself before we begin printing it.*)
  let p, s = Locations.prepare_prog p in
  let model_namespace_str = namespace p in
  let model_contents =
    usings @ Locations.gen_globals s
    @ collect_functors_functions p
    @ if !standalone_functions then [] else [lower_model p] in
  let model_namespace = Namespace (model_namespace_str, model_contents) in
  let global_fns =
    if !standalone_functions then
      List.concat_map
        ~f:(lower_standalone_fun_def model_namespace_str)
        p.functions_block
    else
      new_model_boilerplate p.prog_name
      @ register_map_rect_functors model_namespace_str in
  [version; includes; model_namespace] @ global_fns

module Testing = struct
  open Fmt

  let%expect_test "model public basics" =
    model_public_basics "foobar"
    |> str "@[<v>%a" (list ~sep:cut Cpp.Printing.pp_defn)
    |> print_endline ;
    [%expect
      {|
      inline std::string model_name() const final
      {
        return "foobar";
      }
      inline std::vector<std::string> model_compile_info() const noexcept
      {
        return std::vector<std::string>{"stanc_version = %%NAME%%3 %%VERSION%%",
                 "stancflags = "};
      } |}]

  let%expect_test "boilerplate" =
    new_model_boilerplate "foobar"
    |> str "@[<v>%a" (list Cpp.Printing.pp_defn)
    |> print_endline ;
    [%expect
      {|
        using stan_model = foobar_namespace::foobar;
        #ifndef USING_R
        // Boilerplate
        stan::model::model_base&
        new_model(stan::io::var_context& data_context, unsigned int seed,
                  std::ostream* msg_stream)
        {
          stan_model* m = new stan_model(data_context, seed, msg_stream);
          return *m;
        }
        stan::math::profile_map& get_stan_profile_data()
        {
          return foobar_namespace::profiles__;
        }
        #endif |}]

  let%expect_test "emplace names" =
    gen_indexing_loop "foo" [Var "N"] emplace_name
    |> str "@[<v>%a" (list ~sep:cut Cpp.Printing.pp_stmt)
    |> print_endline ;
    [%expect
      {|
      for(int sym1__ = 1; sym1__ <= N; ++sym1__) {
        param_names__.emplace_back(std::string() + "foo" + '.' +
          std::to_string(sym1__));
      } |}]

  let%expect_test "complex names" =
    gen_indexing_loop "foo" [Var "N"; Var "D"] emplace_complex_name
    |> str "@[<v>%a" (list ~sep:cut Cpp.Printing.pp_stmt)
    |> print_endline ;
    [%expect
      {|
          for(int sym1__ = 1; sym1__ <= N; ++sym1__) {
            for(int sym2__ = 1; sym2__ <= D; ++sym2__) {
              param_names__.emplace_back(std::string() + "foo" + std::to_string(sym1__)
                + '.' + std::to_string(sym2__) + '.' + "real");
              param_names__.emplace_back(std::string() + "foo" + std::to_string(sym1__)
                + '.' + std::to_string(sym2__) + '.' + "imag");
            }
          } |}]
end
