open Syntax
exception RuntimeError of string

let rerr (msg : string) = raise(RuntimeError ("Error: " ^ msg))

let stringOfType = function
  |  IntType -> "IntType"
  | FloatType -> "FloatType"
  | BoolType -> "BoolType"
  | VoidType -> "VoidType"
  | NullType -> "NullType"
  | LocType -> "LocType"
  | ObjectType(obj) -> "ObjectType(" ^ obj ^ ")"

let stringOfValue = function
  | NullV -> "NullV"
  | IntV(i) -> "Int " ^ (string_of_int i)
  | FloatV(f) -> "Float " ^ (string_of_float f)
  | BoolV(b) -> "Bool " ^ (string_of_bool b)
  | VoidV -> "Void"
  | LocV(l) -> "Location(" ^ (string_of_int l) ^ ")"

let stringOfExp = function _ -> "exp" (*TODO: implement this method*)

let stringOfEnv env =
  let stringList = (Environment.map
                      (fun id typeValue -> "(" ^ id ^ " {typ = " ^ (stringOfType typeValue.typ) ^
                                           "; value = " ^ (stringOfValue typeValue.value) ^ "})") env) in
  (String.concat ", " stringList)

let rec getParent obj prog = match obj with
  | ObjectType(cn) -> (match prog with Program classList -> getParentAux cn classList)
  | primitiveType -> rerr ("Primitive type " ^ (stringOfType primitiveType) ^ "has no base class.")
and
  getParentAux cn = function
  | Class(c, p, _, _) :: tl -> if c = cn then (ObjectType p) else getParentAux cn tl
  | [] ->  rerr (cn ^ " class not declared inside program.")


let rec getFieldList obj prog = match obj with
  | ObjectType "Object" -> []
  | ObjectType(cn) ->  let p = (getParent obj prog) in (getFieldList p prog) @ (getFieldListAux1 cn prog)
  | primitiveType -> rerr ("Primitive type " ^ (stringOfType primitiveType) ^ "has no fields.")
and getFieldListAux1 cn = function Program classList -> getFieldListAux2 cn classList

and getFieldListAux2 (cn : Syntax.id) (classList : Syntax.classDeclaration list) = match classList with
  | [Class(c, _, fields, _)] -> if c = cn then fields else []
  | Class(c, _, fields, _) :: tl -> if c = cn then fields else getFieldListAux2 cn tl
  | [] -> [] (*think about this case*)

let getTypeVar var (env : Syntax.typeValue Environment.t) : Syntax.typ =
  try (Environment.lookup var env).typ with Environment.Not_bound -> Raise_error.unboundVar var

let getTypeList idList env =
  List.map (fun id -> getTypeVar id env) idList

(* let getTypeListStatic idList tenv =
   List.map (fun id ->  ) *)

let getTypeVal = function
  (* TODO: Think about Null and Void *)
  | IntV _ -> IntType
  | FloatV _ -> FloatType
  | BoolV _ -> BoolType
  | VoidV -> VoidType
  | LocV _ -> LocType
  | NullV -> NullType
let getTypeField objType fn prog = match objType with
  | ObjectType(cn) -> (let fieldList = getFieldList objType prog in
                       try let t = List.assoc fn fieldList in Some(t)  with Not_found -> None)
  | _ -> None

let initValue = function
  | IntType -> IntV(0)
  | FloatType -> FloatV(0.0)
  | BoolType -> BoolV(true)
  | VoidType -> VoidV
  | ObjectType(_) -> NullV
  | LocType -> NullV
  | NullType -> NullV

let isValue = function
  | Value(_) -> true
  | _ -> false
let isObjectType = function
  | ObjectType _ -> true
  | _ -> false
let rec definedInProgAux (id : Syntax.id) (classList : Syntax.classDeclaration list) : bool = match classList with
  | Class(c, _, _, _) :: tl -> if c = id then true else definedInProgAux id tl
  | [] -> false

let definedInProg id = function Program classList -> if id = "Object" then true else definedInProgAux id classList


let rec firstUnboundVariable params env = match params with
  | id :: tl -> if Environment.isIn id env then firstUnboundVariable tl env else Some(id)
  | [] -> None

let rec constructHierarchyList objType prog= match objType with
  | ObjectType("Object") -> [objType]
  | ObjectType(cn) -> let p = (getParent objType prog) in [objType] @ (constructHierarchyList p prog)
  | primitiveType -> rerr ("Can not construct hierarchy for primitive type " ^ (stringOfType primitiveType))


let rec findFirstIntersection list1 list2 = try let first = (List.find (fun x-> List.exists (fun y-> y=x) list2) list1) in Some(first) with Not_found -> None

let rec leastMaxType t1 t2 prog = match t1, t2 with
  | ObjectType(cn1), ObjectType(cn2) -> let h1 = constructHierarchyList t1 prog in let h2 = constructHierarchyList t2 prog in findFirstIntersection h1 h2
  | primitive1, primitive2 -> if primitive1 = primitive2 then Some(t1) else None

let rec isSubtype t1 t2 prog= match t1, t2 with
  | NullType, ObjectType(_) -> true
  | ObjectType _, ObjectType("Object") -> true
  | ObjectType("Object"), _ -> false
  | LocType, ObjectType _ -> true
  | ObjectType(cn1), ObjectType(cn2) -> let p = (getParent t1 prog) in if t2 = p then true else (isSubtype p t2 prog)
  | a, b -> if a = b then true else (print_endline ("IN ISSUBTYPE(this should never happen :) )" ^ (stringOfType a) ^ "," ^ (stringOfType b)); false)

let rec checkFieldsTypes fields types prog = match fields, types with
  | (f,tf) :: tlf, tv :: tlt -> if isSubtype tv tf prog then checkFieldsTypes tlf tlt prog else Some(f)
  | [], [] -> None
  | _ -> raise(RuntimeError ("Default case reached in Utils.checkFieldsTypes")) (*TODO better treat this case*)


let rec createFieldEnv (fields : (Syntax.id * Syntax.typ) list) idList (env : Syntax.typeValue Environment.t) = match fields, idList with
  | (f, tf) :: tlf, id :: tl -> let v = (Environment.lookup id env) in
    Environment.extend f v (createFieldEnv tlf tl env)
  | [], [] -> Environment.empty
  | _ ->  raise(RuntimeError ("Default case reached in Utils.createFieldEnv")) (*TODO better treat this case*)

let isIntOperator = function IPlus|IMinus|IMultiply|IDivide  -> true | _ -> false
let isFloatOperator = function FPlus|FMinus|FMultiply|FDivide -> true | _ -> false
let isCompOperator = function Less| LessEqual| EqEqual| GreaterEqual| Greater| NotEqual -> true | _ -> false
let isBoolOperator = function And|Or -> true | _ -> false

let raiseDifferentTypeExpErr exp expectedType actualType = raise(RuntimeError ("Error 5: Expression " ^ (stringOfExp exp) ^
                                                                               "has type " ^ (stringOfType actualType) ^ " but an expression was expected of type " ^ (stringOfType expectedType)) )
