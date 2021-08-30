open Core_kernel

(** A primitive (basic) transformation. One or many of these makes up a transformation *)
type 'e primitive =
  | Identity
  | Lower of 'e
  | Upper of 'e
  | LowerUpper of 'e * 'e
  | Offset of 'e
  | Multiplier of 'e
  | OffsetMultiplier of 'e * 'e
  | Ordered
  | PositiveOrdered
  | Simplex
  | UnitVector
  | CholeskyCorr
  | CholeskyCov
  | Correlation
  | Covariance
[@@deriving sexp, compare, map, hash, fold]

(** Transformations (constraints) for global variable declarations *)
type 'e t =
  | Single of 'e primitive
  (* given in CONSTRAINING ORDER
   * e.g. x_con = f(g(x_unc)) is written [g;f]
   * = "When constraining, first do g, then do f." 
   *)
  | Chain of 'e primitive list
[@@deriving sexp, compare, map, hash, fold]

let fold_prims f acc = function
  | Single t -> f acc t
  | Chain ts -> List.fold ts ~init:acc ~f

let primitive_has_check = function
  | Identity | Offset _ | Multiplier _ | OffsetMultiplier _ -> false
  | _ -> true

let has_transform = function Single Identity -> false | _ -> true
let list = function Single t -> [t] | Chain ts -> ts
