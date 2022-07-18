open! Core_kernel

type template_parameter =
  | Typename of string  (** The name of a template typename *)
  | Require of string * string
      (** A C++ type trait and the name which needs to satisfy that *)
  | Bool of string  (** A named boolean template type *)

type identifier = string

type expr =
  | Literal of string
  | Var of identifier
  | FunCall of identifier * expr list
  | MethodCall of expr * identifier * expr list
  | Assign of identifier * expr
  | Incr of identifier

type type_ =
  | Auto
  | Int
  | Double
  | Complex of type_
  | TemplateType of string
  | Vector of type_
  | Class of identifier
  | Matrix of type_ * int * int
  | TypeTrait of string * type_ list

type var_defn =
  { const: bool [@default false]
  ; type_: type_
  ; name: identifier
  ; init: expr option [@default None] }
[@@deriving make]

type stmt =
  | Expression of expr
  | VarDef of var_defn
  | For of expr * expr * expr * stmt
  | While of expr * stmt
  | IfElse of expr * stmt * stmt
  | Block of stmt list
  | Comment of string

type return_ty = Type of type_ | Void

type fun_defn =
  { inline: bool [@default false]
  ; const: bool [@default false]
  ; templates: template_parameter list [@default []]
  ; name: identifier
  ; return_type: return_ty
  ; args: type_ * string list
  ; body: stmt list option }
[@@deriving make]

type defn =
  | FunDef of fun_defn
  | Struct of template_parameter option * identifier * defn list
  | TopVarDef of var_defn
  | TopComment of string
  | Using of string
  | Namespace of string * defn list

type program = defn list

type found_functor =
  { struct_template: template_parameter option
  ; arg_templates: template_parameter list
  ; signature: string
  ; defn: string }

module Printing = struct
  open Fmt

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

  let rec pp_expr ppf e =
    match e with
    | Literal s -> pf ppf "@[%a@]" text s
    | Var id -> string ppf id
    | FunCall (fn, es) -> pf ppf "%s(@[%a@])" fn (list pp_expr) es
    | MethodCall (e, fn, es) ->
        pf ppf "%a.%s(@[%a@])" pp_expr e fn (list pp_expr) es
    | Assign (id, e) -> pf ppf "%s = %a" id pp_expr e
    | Incr id -> pf ppf "++%s" id
end
