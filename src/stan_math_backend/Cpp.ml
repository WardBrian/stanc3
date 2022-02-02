open! Core_kernel
open Fmt

type template =
  | Typename of string
  | Require of string * string
  | Bool of string

let pp_template ppf template =
  match template with
  | Typename t -> pf ppf "typename %s" t
  | Require (r, t) -> pf ppf "%s<%s>*" r t
  | Bool s -> pf ppf "bool %s" s

let pp_template_defaults ppf template =
  match template with
  | Require _ -> pf ppf "%a = nullptr" pp_template template
  | _ -> pp_template ppf template

let pp_templates ~defaults ppf templates =
  match templates with
  | [] -> ()
  | _ ->
      pf ppf "template <@[%a@]>@ "
        (list ~sep:comma
           (if defaults then pp_template_defaults else pp_template) )
        templates

type identifier = string

type type_ =
  | Auto
  | Int
  | Double
  | Complex of type_
  | TemplateType of string
  | Vector of type_
  | Class of identifier
  | Matrix of type_ * int * int

type return_ty = Type of type_ | ReturnType of type_ list

type expr =
  | Literal of string
  | Var of identifier
  | FunCall of identifier * expr list
  | MethodCall of expr * identifier * expr list
  | Assign of identifier * expr
  | Incr of identifier

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
  | Block of stmt list

type fun_defn =
  { inline: bool [@default false]
  ; const: bool [@default false]
  ; templates: template list [@default []]
  ; name: identifier
  ; return_type: return_ty
  ; args: type_ * string list
  ; body: stmt list option }
[@@deriving make]

type defn =
  | FunDef of fun_defn
  | Struct of identifier * defn list
  | TopVarDef of var_defn

type found_functor =
  { struct_template: template option
  ; arg_templates: template list
  ; signature: string
  ; defn: string }
