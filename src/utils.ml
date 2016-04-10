open Syntax

let getTypeVar var (env:Syntax.typeValue Environment.t): Syntax.typ =
  (Environment.lookup var env).typ

let getTypeVal = function
  (* TODO: Think about Null and Void *)
    IntV _ -> IntType
  | FloatV _ -> FloatType
  | BoolV _ -> BoolType
  | VoidV -> VoidType
  | _ -> failwith "Think about Null and Void "

let isSubtype type1 type2 = true

let initValue = function
    IntType -> IntV(0)
  | FloatType -> FloatV(0.0)
  | BoolType -> BoolV(true)
  | VoidType -> VoidV
  | ObjectType(_) -> NullV

let isValue = function
    Value(_) -> true
  | _ -> false
