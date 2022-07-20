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
    | SComplexVector size | SComplexRowVector size ->
        let typ = lower_st st adtype in
        typ
        |::? ("Constant", [lower_expr size; initialize_value SComplex adtype])
    | SVector (_, size) | SRowVector (_, size) ->
        let typ = lower_st st adtype in
        typ |::? ("Constant", [lower_expr size; init_nan])
    | SMatrix (_, d1, d2) ->
        let typ = lower_st st adtype in
        typ |::? ("Constant", [lower_expr d1; lower_expr d2; init_nan])
    | SComplexMatrix (d1, d2) ->
        let typ = lower_st st adtype in
        typ
        |::? ( "Constant"
             , [lower_expr d1; lower_expr d2; initialize_value SComplex adtype]
             )
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
    | SVector (AoS, size) | SRowVector (AoS, size) ->
        let typ = lower_st st adtype in
        typ |::? ("Constant", [lower_expr size; init_nan])
    | SComplexVector size | SComplexRowVector size ->
        let typ = lower_st st adtype in
        typ
        |::? ("Constant", [lower_expr size; initialize_value SComplex adtype])
    | SMatrix (AoS, d1, d2) ->
        let typ = lower_st st adtype in
        typ |::? ("Constant", [lower_expr d1; lower_expr d2; init_nan])
    | SComplexMatrix (d1, d2) ->
        let typ = lower_st st adtype in
        typ
        |::? ( "Constant"
             , [lower_expr d1; lower_expr d2; initialize_value SComplex adtype]
             )
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
let lower_assign_sized st adtype initialize =
  if initialize then Some (initialize_value st adtype) else None

let (*rec*) lower_statement Stmt.Fixed.{pattern; meta} : stmt list =
  let remove_promotions (e : 'a Expr.Fixed.t) =
    (* assignment handles one level of promotion internally, don't do it twice *)
    match e.pattern with Promotion (e, _, _) -> e | _ -> e in
  let wrap_e e = [Expression e] in
  let location =
    match pattern with
    | Block _ | SList _ | Decl _ | Skip | Break | Continue -> []
    | _ -> Locations.create_loc_assignment meta in
  location
  @
  match pattern with
  | Assignment
      ( (vident, _, [])
      , ( {pattern= FunApp (CompilerInternal (FnReadData | FnReadParam _), _); _}
        as rhs ) ) ->
      Assign (Var vident, lower_expr rhs) |> wrap_e
  | Assignment
      ((vident, _, []), ({meta= Expr.Typed.Meta.{type_= UInt; _}; _} as rhs))
   |Assignment
      ((vident, _, []), ({meta= Expr.Typed.Meta.{type_= UComplex; _}; _} as rhs))
   |Assignment ((vident, _, []), ({meta= {type_= UReal; _}; _} as rhs)) ->
      Assign (Var vident, lower_expr (remove_promotions rhs)) |> wrap_e
  | Assignment ((assignee, UInt, idcs), rhs)
   |Assignment ((assignee, UReal, idcs), rhs)
    when List.for_all ~f:is_single_index idcs ->
      Assign (lower_indexed_simple (to_mir_var assignee) idcs, lower_expr rhs)
      |> wrap_e
  | _ -> failwith "todo"
(* | Assignment ((assignee, _, idcs), rhs) ->
       (* XXX I think in general we don't need to do a deepcopy if e is nested
          inside some function call - the function should get its own copy
          (in all cases???) *)
       let rec maybe_deep_copy e =
         let recurse (e : 'a Expr.Fixed.t) =
           { e with
             Expr.Fixed.pattern= Expr.Fixed.Pattern.map maybe_deep_copy e.pattern
           } in
         match e.pattern with
         | _ when UnsizedType.is_scalar_type (Expr.Typed.type_of e) -> e
         | FunApp (CompilerInternal _, _) -> e
         | (Indexed ({Expr.Fixed.pattern= Var v; _}, _) | Var v)
           when v = assignee ->
             { e with
               Expr.Fixed.pattern= FunApp (CompilerInternal FnDeepCopy, [e]) }
         | _ -> recurse e in
       let rhs = maybe_deep_copy (remove_promotions rhs) in
       pf ppf "@[<hov 2>stan::model::assign(@,%s,@ %a,@ %S%s%a@]);" assignee
         pp_expr rhs
         (str "assigning variable %s" assignee)
         (if List.length idcs = 0 then "" else ", ")
         pp_indexes idcs
   | TargetPE e -> pf ppf "@[<hov 2>lp_accum__.add(@,%a@]);" pp_expr e
   | NRFunApp (CompilerInternal FnPrint, args) ->
       let pp_arg ppf a =
         pf ppf "stan::math::stan_print(pstream__, %a);" pp_expr a in
       let args = args @ [Expr.Helpers.str "\n"] in
       pf ppf "if (pstream__) %a" pp_block (list ~sep:cut pp_arg, args)
   | NRFunApp (CompilerInternal FnReject, args) ->
       let err_strm = "errmsg_stream__" in
       let add_to_string ppf e = pf ppf "%s << %a;" err_strm pp_expr e in
       pf ppf "std::stringstream %s;@," err_strm ;
       pf ppf "%a@," (list ~sep:cut add_to_string) args ;
       pf ppf "throw std::domain_error(%s.str());" err_strm
   | NRFunApp (CompilerInternal (FnCheck {trans; var_name; var}), args) ->
       Option.iter (check_to_string trans) ~f:(fun check_name ->
           let function_arg = Expr.Helpers.variable "function__" in
           if List.length args = 0 then
             pf ppf "%s(@[<hov 2>%a, %a,@, %a@]);"
               ("stan::math::check_" ^ check_name)
               pp_expr function_arg pp_expr
               (Expr.Helpers.str var_name)
               pp_expr var
           else
             pf ppf "%s(@[<hov 2>%a, %a,@, %a,@, %a@]);"
               ("stan::math::check_" ^ check_name)
               pp_expr function_arg pp_expr
               (Expr.Helpers.str var_name)
               pp_expr var (list ~sep:comma pp_expr) args )
   | NRFunApp (CompilerInternal (FnWriteParam {unconstrain_opt; var}), _) -> (
     match
       (unconstrain_opt, Option.bind ~f:constraint_to_string unconstrain_opt)
     with
     (* When the current block or this transformation doesn't require unconstraining,
        use vanilla write *)
     | None, _ | _, None -> pf ppf "@[<hov 2>out__.write(@,%a);@]" pp_expr var
     (* Otherwise, use stan::io::serializer's write_free functions *)
     | Some trans, Some unconstrain_string ->
         let unconstrain_args = transform_args trans in
         pf ppf "@[<hov 2>out__.write_free_%s(@,%a);@]" unconstrain_string
           (list ~sep:comma pp_expr)
           (unconstrain_args @ [var]) )
   | NRFunApp (CompilerInternal f, args) ->
       let fname, extra_args = trans_math_fn f in
       pf ppf "%s(@[<hov>%a@]);" fname (list ~sep:comma pp_expr)
         (extra_args @ args)
   | NRFunApp (StanLib (fname, _, _), args) ->
       pf ppf "%s(@[<hov>%a@]);"
         (stan_namespace_qualify fname)
         (list ~sep:comma pp_expr) args
   | NRFunApp (UserDefined (fname, suffix), args) ->
       pf ppf "%a;" pp_user_defined_fun (fname, suffix, args)
   | Break -> string ppf "break;"
   | Continue -> string ppf "continue;"
   | Return e -> pf ppf "@[<hov 4>return %a;@]" (option pp_expr) e
   | Skip -> string ppf ";"
   | IfElse (cond, ifbranch, elsebranch) ->
       let pp_else ppf x = pf ppf "else %a" pp_statement x in
       pf ppf "if (@[<hov>%a@]) %a %a" pp_bool_expr cond pp_block_s ifbranch
         (option pp_else) elsebranch
   | While (cond, body) ->
       pf ppf "while (@[<hov>%a@]) %a" pp_bool_expr cond pp_block_s body
   | For
       { body=
           { pattern=
               Assignment
                 (_, {pattern= FunApp (CompilerInternal (FnReadParam _), _); _})
           ; _ } as body
       ; _ } ->
       pp_statement ppf body
       (* Skip For loop part, just emit body due to the way FnReadParam emits *)
   | For {loopvar; lower; upper; body} ->
       pp_for_loop ppf (loopvar, lower, upper, pp_statement, body)
   | Profile (name, ls) -> pp_profile ppf (pp_stmt_list, name, ls)
   | Block ls -> pp_block ppf (pp_stmt_list, ls)
   | SList ls -> pp_stmt_list ppf ls
   | Decl {decl_adtype; decl_id; decl_type; initialize; _} ->
       pp_decl ppf (decl_id, decl_type, decl_adtype, initialize) *)

module Testing = struct
  let%expect_test "set size mat array" =
    let int = Expr.Helpers.int in
    Fmt.str "@[<v>%a@]"
      (Fmt.option Cpp.Printing.pp_expr)
      (lower_assign_sized
         (SArray (SArray (SMatrix (AoS, int 2, int 3), int 4), int 5))
         DataOnly false )
    |> print_endline ;
    [%expect {| |}]

  let%expect_test "set size mat array" =
    let int = Expr.Helpers.int in
    Fmt.str "@[<v>%a@]"
      (Fmt.option Cpp.Printing.pp_expr)
      (lower_assign_sized
         (SArray (SArray (SMatrix (AoS, int 2, int 3), int 4), int 5))
         DataOnly true )
    |> print_endline ;
    [%expect
      {|
    std::vector<std::vector<Eigen::Matrix<double,-1,-1>>>(5,
      std::vector<Eigen::Matrix<double,-1,-1>>(4,
        Eigen::Matrix<double,-1,-1>::Constant(2, 3,
                                              std::numeric_limits<double>::quiet_NaN(
                                                )))) |}]
end
