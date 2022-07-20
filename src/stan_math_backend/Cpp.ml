open! Core_kernel

type identifier = string [@@deriving sexp]

type type_ =
  | Auto
  | Void
  | Int
  | Double
  | Complex of type_
  | TemplateType of identifier
  | Vector of type_
  | Array of type_ * int
  | Typename of identifier
  | Matrix of type_ * int * int
  | Ref of type_
  | Const of type_
  | Pointer of type_
  | TypeTrait of string * type_ list
      (** e.g. stan::promote_scalar, stan:base_type *)
[@@deriving sexp]

module Types = struct
  let local_scalar = Typename "local_scalar_t__"
  let std_vector t = Vector t
  let complex s = Complex s
  let vector s = Matrix (s, -1, 1)
  let row_vector s = Matrix (s, 1, -1)
  let matrix s = Matrix (s, -1, -1)
  let string = Typename "std::string"
  let const_ref t = Const (Ref t)
  let str_array i = Array (Const (Pointer (Typename "char")), i)
end

type operator = Multiply | Divide | Add | Subtract | Eq | LEq | GEq | And | Or
[@@deriving sexp]

type unary_op = PMinus | Incr [@@deriving sexp]

type expr =
  | Literal of string
  | Var of identifier
  | Parens of expr
  | FunCall of identifier * type_ list * expr list
  | MethodCall of expr * identifier * type_ list * expr list
  | StaticMethodCall of type_ * identifier * type_ list * expr list
  | Constructor of type_ * expr list
  | InitalizerExpr of type_ * expr list
  | TernaryIf of expr * expr * expr
  | Cast of type_ * expr
  | Index of expr * expr
  | Assign of expr * expr (* NB: Not all exprs are valid lvalues! *)
  | Unary of unary_op * expr
  | StreamInsertion of expr * expr list
  | BinOp of expr * operator * expr
[@@deriving sexp]

module Exprs = struct
  (** Some helper values and functions *)

  let method_call obj name templates args =
    match obj with
    | Var _ -> MethodCall (obj, name, templates, args)
    | _ -> MethodCall (Parens obj, name, templates, args)

  let literal_string s = Literal ("\"" ^ s ^ "\"")
  let std_vector_expr t es = InitalizerExpr (Vector t, es)
  let fun_call s es = FunCall (s, [], es)
  let templated_fun_call s ts es = FunCall (s, ts, es)
  let quiet_NaN = fun_call "std::numeric_limits<double>::quiet_NaN" []
  let int_min = fun_call "std::numeric_limits<int>::min" []
end

module Expression_syntax = struct
  (** Some operators to make streams and method calls look more
      like the resultant C++ *)

  include Exprs

  let ( << ) a b = StreamInsertion (a, b)
  let ( .@!() ) obj name = method_call obj name [] []
  let ( .@?() ) obj (name, args) = method_call obj name [] args

  let ( .@<>() ) obj (name, templates, args) =
    method_call obj name templates args

  let ( |::! ) ty name = StaticMethodCall (ty, name, [], [])
  let ( |::? ) ty (name, args) = StaticMethodCall (ty, name, [], args)

  let ( |::<> ) ty (name, templates, args) =
    StaticMethodCall (ty, name, templates, args)

  let ( + ) a b = BinOp (a, Add, b)
  let ( - ) a b = BinOp (a, Subtract, b)
  let ( * ) a b = BinOp (a, Multiply, b)
  let ( / ) a b = BinOp (a, Divide, b)
end

type var_defn =
  { static: bool [@default false]
  ; constexpr: bool [@default false]
  ; type_: type_
  ; name: identifier
  ; init: expr option [@default None] }
[@@deriving make, sexp]

type stmt =
  | Expression of expr
  | VarDef of var_defn
  | For of var_defn * expr * expr * stmt
  | While of expr * stmt
  | IfElse of expr * stmt * stmt
  | TryCatch of stmt * var_defn * stmt
  | Block of stmt list
  | Return of expr
  | Break
  | Continue
  | Using of string * string option
  | Comment of string
[@@deriving sexp]

module Stmts = struct
  let rethrow_located stmts =
    let exn =
      make_var_defn
        ~type_:(Types.const_ref (Typename "std::exception"))
        ~name:"e" () in
    TryCatch
      ( Block stmts
      , exn
      , Block
          [ Expression
              (FunCall
                 ( "stan::lang::rethrow_located"
                 , []
                 , [ Var "e"
                   ; Index (Var "locations_array__", Var "current_statement__")
                   ] ) ) ] )

  let fori loopvar lower upper body =
    let init = make_var_defn ~type_:Int ~name:loopvar ~init:(Some lower) () in
    let stop = BinOp (Var loopvar, LEq, upper) in
    let incr = Unary (Incr, Var loopvar) in
    For (init, stop, incr, body)
end

type template_parameter =
  | Typename of string  (** The name of a template typename *)
  | Require of string * string
      (** A C++ type trait and the name which needs to satisfy that *)
  | Bool of string  (** A named boolean template type *)
[@@deriving sexp]

type return_ty =
  {type_: type_; inline: bool [@default false]; const: bool [@default false]}
[@@deriving sexp, make]

type fun_defn =
  { templates: template_parameter list [@default []]
  ; name: identifier
  ; return_type: return_ty
  ; args: type_ * string list
  ; const: bool [@default false]
  ; body: stmt list option }
[@@deriving make, sexp]

type defn =
  | FunDef of fun_defn
  | Struct of template_parameter option * identifier * defn list
  | TopVarDef of var_defn
  | TopComment of string
  | TopUsing of string * string option
  | Namespace of identifier * defn list
[@@deriving sexp]

type program = defn list [@@deriving sexp]

type found_functor =
  { struct_template: template_parameter option
  ; arg_templates: template_parameter list
  ; signature: string
  ; defn: string }

module Printing = struct
  open Fmt

  let pp_identifier ppf = string ppf

  let rec pp_type_ ppf t =
    match t with
    | Auto -> string ppf "auto"
    | Void -> string ppf "void"
    | Int -> string ppf "int"
    | Double -> string ppf "double"
    | Complex t -> pf ppf "std::complex<%a>" pp_type_ t
    | TemplateType id -> pp_identifier ppf id
    | Vector t -> pf ppf "std::vector<@[@,%a@]>" pp_type_ t
    | Array (t, i) -> pf ppf "std::array<@[@,%a,@ %i@]>" pp_type_ t i
    | Typename id -> pp_identifier ppf id
    | Matrix (t, i, j) -> pf ppf "Eigen::Matrix<@[%a,%i,%i@]>" pp_type_ t i j
    | Const t -> pf ppf "const %a" pp_type_ t
    | Ref t -> pf ppf "%a&" pp_type_ t
    | Pointer t -> pf ppf "%a*" pp_type_ t
    | TypeTrait (s, ts) -> pf ppf "%s<@[%a@]>" s (list ~sep:comma pp_type_) ts

  let pp_template_parameter ppf template_parameter =
    match template_parameter with
    | Typename param_name -> pf ppf "typename %s" param_name
    | Require (requirement, param_name) ->
        pf ppf "stan::require_t<%s<%s>>*" requirement param_name
    | Bool param_name -> pf ppf "bool %s" param_name

  let pp_requires ~default ppf requires =
    match requires with
    | [] -> ()
    | _ ->
        let pp_require ppf (trait, name) = pf ppf "%s<%s>" trait name in
        pf ppf ",@ stan::require_all_t<@[%a@]>*%s"
          (list ~sep:comma pp_require)
          requires
          (if default then " = nullptr" else "")

  (**
   Pretty print a full C++ `template <parameter-list>`
  *)
  let pp_template ~default ppf template_parameters =
    match template_parameters with
    | [] -> ()
    | _ ->
        let templates, requires =
          List.partition_map template_parameters ~f:(function
            | Require (trait, name) -> Second (trait, name)
            | Typename name -> First ("typename " ^ name)
            | Bool name -> First ("bool " ^ name) ) in
        pf ppf "template <@[%a%a@]>@ " (list ~sep:comma string) templates
          (pp_requires ~default) requires

  let pp_operator ppf = function
    | Multiply -> string ppf "*"
    | Divide -> string ppf "/"
    | Add -> string ppf "+"
    | Subtract -> string ppf "-"
    | Eq -> string ppf "=="
    | LEq -> string ppf "<="
    | GEq -> string ppf ">="
    | And -> string ppf "&&"
    | Or -> string ppf "||"

  let pp_unary_op ppf = function
    | PMinus -> string ppf "-"
    | Incr -> string ppf "++"

  let rec pp_expr ppf e =
    let maybe_templates ppf ts =
      if List.length ts = 0 then nop ppf ()
      else pf ppf "<@,%a>" (list ~sep:comma pp_type_) ts in
    match e with
    | Literal s -> pf ppf "%a" text s
    | Var id -> string ppf id
    | Parens e -> (parens pp_expr) ppf e
    | Cast (t, e) -> pf ppf "(%a)%a" pp_type_ t pp_expr e
    | Constructor (t, es) ->
        pf ppf "@[<hov 2>%a(%a)@]" pp_type_ t (list ~sep:comma pp_expr) es
    | InitalizerExpr (t, es) ->
        pf ppf "%a{@[%a@]}" pp_type_ t (list ~sep:comma pp_expr) es
    | StreamInsertion (e, es) ->
        pf ppf "%a <<@[@ %a@]" pp_expr e (list ~sep:comma pp_expr) es
    | FunCall (fn, tys, es) ->
        pf ppf "@[<hov 2>%s%a(@,%a@])" fn maybe_templates tys
          (list ~sep:comma pp_expr) es
    | MethodCall (e, fn, tys, es) ->
        pf ppf "@[<hov 2>%a.%s%a(@[@,%a@])@]" pp_expr e fn maybe_templates tys
          (list ~sep:comma pp_expr) es
    | StaticMethodCall (ty, fn, tys, es) ->
        pf ppf "@[<hov 2>%a::%s%a(@[@,%a@])@]" pp_type_ ty fn maybe_templates
          tys (list ~sep:comma pp_expr) es
    | TernaryIf (e1, e2, e3) ->
        pf ppf "%a ? %a : %a" pp_expr e1 pp_expr e2 pp_expr e3
    | Index (e1, e2) -> pf ppf "%a[%a]" pp_expr e1 pp_expr e2
    | Assign (e1, e2) -> pf ppf "%a = %a" pp_expr e1 pp_expr e2
    | Unary (op, e) -> pf ppf "%a%a" pp_unary_op op pp_expr e
    | BinOp (e1, op, e2) ->
        pf ppf "@[<hov 2>%a %a@ %a@]" pp_expr e1 pp_operator op pp_expr e2

  let pp_var_defn ppf {static; constexpr; type_; name; init} =
    let static = if static then "static " else "" in
    let constexpr = if constexpr then "constexpr " else "" in
    (* ordering? *)
    pf ppf "@[%s%s%a %s%a@]" static constexpr pp_type_ type_ name
      (option (fun ppf e -> pf ppf "@[<hov>@ =@ %a@]" pp_expr e))
      init

  let rec pp_stmt ppf s =
    match s with
    | Expression e -> pf ppf "%a;" pp_expr e
    | Return e -> pf ppf "return %a;" pp_expr e
    | Break -> string ppf "break;"
    | Continue -> string ppf "continue;"
    | VarDef vd -> pf ppf "%a;" pp_var_defn vd
    | For (init, cond, incr, s) ->
        pf ppf "@[<v 2>for(@[<hov>%a; %a; %a@])@ @[%a@]@]" pp_var_defn init
          pp_expr cond pp_expr incr pp_stmt s
    | While (e, s) ->
        pf ppf "@[<v 2>while(@[%a@])@ @[%a@]@]" pp_expr e pp_stmt s
    | IfElse (cond, thn, els) ->
        pf ppf "@[<v 2>if(@[%a@])@ @[%a@]@]@,@[<v 2>else @[%a@]@]" pp_expr cond
          pp_stmt thn pp_stmt els
    | Block stmts ->
        pf ppf "@[<v>@[<v 2>{@,%a@]@,}@]" (list ~sep:cut pp_stmt) stmts
    | Using (s, init) ->
        pf ppf "using %s%a;" s
          (option (fun ppf defn -> pf ppf "= %s" defn))
          init
    | Comment s -> pf ppf "/@[<v>*@[@ %a@]@,@]*/" text s
    | TryCatch (trys, exn, thn) ->
        pf ppf "@[<v>try@ %a@ catch(%a)@ %a@]" pp_stmt trys pp_var_defn exn
          pp_stmt thn
end

module Tests = struct
  let%expect_test "rethrow_located" =
    let s =
      [ Comment "A potentially \n long comment"
      ; Expression (Assign (Var "foo", Literal "3")) ] in
    let rethrow = Stmts.rethrow_located s in
    Printing.pp_stmt Fmt.stdout rethrow ;
    [%expect
      {|
      try
      {
        /* A potentially
           long comment
         */
        foo = 3;
      }
      catch(const std::exception& e)
      {
        stan::lang::rethrow_located(e, locations_array__[current_statement__]);
      } |}]

  let%expect_test "types" =
    let ts =
      let open Types in
      [ matrix (complex local_scalar); str_array 43
      ; std_vector (std_vector Double); const_ref (TemplateType "T0__") ] in
    let open Fmt in
    pf stdout "@[<v>%a@]" (list ~sep:comma Printing.pp_type_) ts ;
    [%expect
      {|
        Eigen::Matrix<std::complex<local_scalar_t__>,-1,-1>,
        std::array<const char*, 43>,
        std::vector<std::vector<double>>,
        const T0__& |}]

  (* This shows off some of the fancy syntax OCaml lets us use,
      like [<<] or [.@()]*)
  let%expect_test "eigen init" =
    let open Expression_syntax in
    let open Types in
    let vector = Constructor (row_vector Double, [Literal "3"]) in
    let values = [Literal "1"; Var "a"; Literal "3"] in
    let e = (vector << values).@!("finished") in
    print_s [%sexp (e : expr)] ;
    print_endline "" ;
    Printing.pp_expr Fmt.stdout e ;
    [%expect
      {|
          (MethodCall
           (Parens
            (StreamInsertion (Constructor (Matrix Double 1 -1) ((Literal 3)))
             ((Literal 1) (Var a) (Literal 3))))
           finished () ())

          (Eigen::Matrix<double,1,-1>(3) << 1, a, 3).finished() |}]
end
