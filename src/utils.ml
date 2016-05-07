open Syntax
open Core.Std
open Exn2

let raiseRuntimeError (msg : string) = raise(RuntimeError ("Error: " ^ msg))

let stringOfType = function
  |  IntType -> "int"
  | FloatType -> "float"
  | BoolType -> "bool"
  | VoidType -> "void"
  | NullType -> "NullType"
  | LocType -> "LocType"
  | ObjectType(obj) -> obj

let rec stringListOfIdTypList = function
  | []-> []
  | hd :: tl -> match hd with
    | (id, typ) -> ((stringOfType typ) ^ " " ^ id) :: stringListOfIdTypList tl

let stringOfValue = function
  | NullV -> "null"
  | IntV(i) -> "Int " ^ (string_of_int i)
  | FloatV(f) -> "Float " ^ (string_of_float f)
  | BoolV(b) -> "Bool " ^ (string_of_bool b)
  | VoidV -> "void"
  | LocV(l) -> "Location(" ^ (string_of_int l) ^ ")"

let stringOfOp = function
  |IPlus -> " + "
  |IMinus -> " - "
  |IDivide -> " / "
  |IMultiply -> " * "
  | _ -> "floatOp"

let rec stringOfExp = function
  | Value(v) -> stringOfValue v
  | Variable(id) -> "variable " ^ id
  | ObjectField(var, field) -> "ObjectField"
  | VariableAssignment(id, exp) -> "VariableAssignment " ^ id ^ " = " ^ "("^ stringOfExp exp ^ ")"
  | ObjectFieldAssignment((var, f), e) -> "ObjectFieldAssignment"
  | Sequence(e1, e2) -> "Sequence(" ^ stringOfExp e1 ^ "),\n(" ^ stringOfExp e2 ^ "/* endsequence */) "
  | BlockExpression(list, exp) -> "BlockExpression {\n" ^ stringOfExp exp ^ "\n}"
  | If (id, et, ee) -> "If (" ^ id ^ ") then \n"^ stringOfExp et ^ "\n else  \n" ^ stringOfExp ee ^ " /* endif */ \n"
  | Operation(e1, op, e2) -> "Operation("^ stringOfExp e1 ^ ")" ^(stringOfOp op)^"(" ^ stringOfExp e2 ^ ")"
  | Negation(e) -> "negation"
  | New(cn, varList) -> "new"
  | While(var, e) -> "while"
  | Cast(cn, var) -> "cast"
  | InstanceOf(var, cn) -> "InstanceOf"
  | MethodCall(cn, mn, params) -> "MethodCall"
  | Ret(v, exp) -> "ret"

let rec stringOfMethods = function
  | [] -> "\n"
  | hd :: tl -> match hd with
    | Method(t, n, args, exp) -> (stringOfType t) ^ " " ^ n ^ " (" ^ (String.concat ~sep:", "  (stringListOfIdTypList args)) ^ ") \n" ^ (stringOfExp exp) ^ "\n/* endmethod */\n"

let stringOfEnv env =
  let stringList = (Environment.map
                      (fun id typeValue -> "(" ^ id ^ " {typ = " ^ (stringOfType typeValue.typ) ^
                                           "; value = " ^ (stringOfValue typeValue.value) ^ "})") env) in
  (String.concat ~sep:", " stringList)

let rec getParent obj prog = match obj with
  | ObjectType(cn) -> (match prog with Program classList -> getParentAux cn classList)
  | primitiveType -> raiseRuntimeError ("Primitive type " ^ (stringOfType primitiveType) ^ "has no base class.")
and
  getParentAux cn = function
  | Class(c, p, _, _) :: tl -> if c = cn then (ObjectType p) else getParentAux cn tl
  | [] ->  raiseRuntimeError (cn ^ " class not declared inside program.")


let rec getFieldList obj prog = match obj with
  | ObjectType "Object" -> []
  | ObjectType(cn) ->  let p = (getParent obj prog) in (getFieldList p prog) @ (getFieldListAux1 cn prog)
  | primitiveType -> raiseRuntimeError ("Primitive type " ^ (stringOfType primitiveType) ^ "has no fields.")

and getFieldListAux1 cn = function Program classList -> getFieldListAux2 cn classList

and getFieldListAux2 (cn : Syntax.id) (classList : Syntax.classDeclaration list) = match classList with
  | [Class(c, _, fields, _)] -> if c = cn then fields else []
  | Class(c, _, fields, _) :: tl -> if c = cn then fields else getFieldListAux2 cn tl
  | [] -> [] (* TODO:think about this case*)

let getTypeOfVar_exn var (env : Syntax.typeValue Environment.t) : Syntax.typ =
  try (Environment.lookup var env).typ with Environment.Not_bound -> Exn2.raiseRuntimeError var

let getTypeList idList env = List.map idList (fun id -> getTypeOfVar_exn id env)

let getTypeOfVal = function
  | IntV _ -> IntType
  | FloatV _ -> FloatType
  | BoolV _ -> BoolType
  | VoidV -> VoidType
  | LocV _ -> LocType
  | NullV -> NullType

let getTypeField objType fn prog = match objType with
  | ObjectType(cn) -> begin
      let fieldList = getFieldList objType prog in
      try
        let (fnf, ftf) = List.find_exn fieldList ~f:(fun (x, _) -> x = fn) in
        Some(ftf)
      with
        Not_found -> None
    end
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

let isLocation = function
  | LocV(_) -> true
  | _ -> false

let isObjectType = function
  | ObjectType _ -> true
  | _ -> false

let rec isDefinedInProgAux (id : Syntax.id) (classList : Syntax.classDeclaration list) : bool = match classList with
  | Class(c, _, _, _) :: tl -> if c = id then true else isDefinedInProgAux id tl
  | [] -> false

let isDefinedInProg id = function Program classList -> if id = "Object" then true else isDefinedInProgAux id classList

let isTypeDeclared t prog = match t with
  |ObjectType cn -> isDefinedInProg cn prog
  |_ -> true

let getMethods obj prog = match prog with
  | Program classList -> (match obj with
      | ObjectType "Object" -> []
      | ObjectType cn -> begin
          try
            let Class(n, pn, _, methods) = List.find_exn classList ~f:(function Class(c, _, _, _) -> cn = c) in
            methods
          with
            Not_found -> raiseRuntimeError ((stringOfType obj) ^ " is not defined inside program.")
        end
      | _ -> [])

let rec getMethodDefinition obj mn prog = match obj with
  | ObjectType cn -> begin
      if cn = "Object" then None
      else
        let methods = (getMethods obj prog) in
        try
          let methodDecl = List.find_exn methods ~f:(function Method(_, n, _, _) -> n = mn) in
          Some(methodDecl)
        with
          Not_found -> getMethodDefinition (getParent obj prog) mn prog
    end
  | _ -> None

let getParentMethods obj prog = let parent = getParent obj prog in getMethods parent prog

let rec firstUnboundVariable params env = match params with
  | id :: tl -> if Environment.isIn id env then firstUnboundVariable tl env else Some(id)
  | [] -> None

let rec constructHierarchyList objType prog = match objType with
  | ObjectType("Object") -> [objType]
  | ObjectType(cn) -> let p = (getParent objType prog) in [objType] @ (constructHierarchyList p prog)
  | primitiveType -> raiseRuntimeError ("Can not construct hierarchy for primitive type " ^ (stringOfType primitiveType))


let rec findFirstIntersection list1 list2 = try
    let first = List.find_exn list1 ~f:(fun x -> List.exists list2 (fun y -> y = x)) in
    Some(first)
  with
    Not_found -> None

let rec leastMaxType t1 t2 prog = match t1, t2 with
  | ObjectType(cn1), ObjectType(cn2) -> let h1 = constructHierarchyList t1 prog in
    let h2 = constructHierarchyList t2 prog in
    findFirstIntersection h1 h2
  | primitive1, primitive2 -> if primitive1 = primitive2 then Some(t1) else None

let rec isSubtype t1 t2 prog = match t1, t2 with
  | NullType, ObjectType(_) -> true
  | ObjectType _, ObjectType("Object") -> true
  | ObjectType("Object"), _ -> false
  | LocType, ObjectType _ -> true
  | ObjectType(cn1), ObjectType(cn2) -> if cn1 = cn2 then true
    else
      let p = getParent t1 prog in
      if t2 = p then true
      else isSubtype p t2 prog
  | a, b -> if a = b then true else false


let rec checkFieldsTypes fields types prog = match fields, types with
  | (f, tf) :: tlf, tv :: tlt -> if isSubtype tv tf prog then checkFieldsTypes tlf tlt prog else Some(f)
  | [], [] -> None
  | _ -> raiseRuntimeError ("Default case reached in Utils.checkFieldsTypes") (*TODO better treat this case*)


let rec createFieldEnv (fields : (Syntax.id * Syntax.typ) list) idList (env : Syntax.typeValue Environment.t) = match fields, idList with
  | (f, tf) :: tlf, id :: tl -> let v = (Environment.lookup id env) in
    Environment.extend f v (createFieldEnv tlf tl env)
  | [], [] -> Environment.empty
  | _ ->  raiseRuntimeError ("Default case reached in Utils.createFieldEnv") (*TODO better treat this case*)

let isIntOperator = function IPlus | IMinus | IMultiply | IDivide  -> true | _ -> false
let isFloatOperator = function FPlus | FMinus | FMultiply | FDivide -> true | _ -> false
let isCompOperator = function Syntax.Less | LessEqual | EqEqual | GreaterEqual | Greater | NotEqual -> true | _ -> false
let isBoolOperator = function And | Or -> true | _ -> false

let eachElementOnce_exn l = List.iteri l ~f:(fun i x ->
    let xList = List.filter l ~f:(fun y -> x = y) in
    if List.length xList = 1 then () else raise (DuplicateElement i))

let methodsOnce_exn class_decl = let Class(_, _, _, methods) = class_decl in
  let methodsNames = List.map methods ~f:(function Method(_, n, _, _) -> n) in
  try
    eachElementOnce_exn methodsNames
  with
    DuplicateElement index -> raise (DuplicateMethod (List.nth_exn methods index))

let fieldsOnce_exn class_decl = let Class(_, _, fields, _) = class_decl in
  let fieldsNames = List.map fields ~f:(function (n, _) -> n) in
  try
    eachElementOnce_exn fieldsNames
  with
    DuplicateElement index -> raise (DuplicateField (List.nth_exn fields index))

let methodName = function Method(_, n, _, _) -> n

let goodOverride m1 m2 prog = match m1, m2 with
  | Method(t1, _, args1, e1), Method(t2, _, args2, e2) -> args1 = args2 && e1 = e2 && isSubtype t1 t2 prog

let goodInheritance cl prog = let Class(cn,pn,_,methods) = cl in
  let parentMethods = getMethods (ObjectType pn) prog in
  List.iter methods ~f:(fun m ->
      List.iter parentMethods ~f:(fun mp ->
          if methodName m = methodName mp then
            if goodOverride m mp prog then ()
            else raise(BadMethodOverriding m)
          else ()))

let compareValues v11 v21 op = match v11, v21 with
  | `Int v1, `Int v2 -> begin  match op with
      | Syntax.Less -> BoolV (v1 < v2)
      | LessEqual -> BoolV (v1 <= v2)
      | EqEqual -> BoolV (v1 = v2)
      | GreaterEqual -> BoolV (v1 >= v2)
      | Greater -> BoolV (v1 > v2)
      | NotEqual -> BoolV (v1 <> v2)
      | _ -> raiseRuntimeError ("This should never happen")
    end
  | `Float v1, `Float v2 -> begin match op with
      | Less -> BoolV (v1 < v2)
      | LessEqual -> BoolV (v1 <= v2)
      | EqEqual -> BoolV (v1 = v2)
      | GreaterEqual -> BoolV (v1 >= v2)
      | Greater -> BoolV (v1 > v2)
      | NotEqual -> BoolV (v1 <> v2)
      | _ -> raiseRuntimeError ("This should never happen")
    end
  | _ -> raiseRuntimeError ("This should never happen")


let rec substVariableName newName name exp = match exp with
  | Value _ ->  exp
  | Variable var -> if var = name then (Variable newName) else exp
  | ObjectField(var, field) -> if var = name then (ObjectField (newName,field)) else exp
  | VariableAssignment(var, e) -> let substExp = substVariableName newName name e in
    if var = name then (VariableAssignment (newName,substExp)) else (VariableAssignment (var,substExp))
  | ObjectFieldAssignment((var, f), e) -> let substExp = substVariableName newName name e in
    if var = name then ObjectFieldAssignment((newName, f), substExp) else ObjectFieldAssignment((var, f), substExp)
  | Sequence(e1, e2) -> Sequence ((substVariableName newName name e1),(substVariableName newName name e2))
  | BlockExpression(list, e) ->  BlockExpression(list,substVariableName newName name e)
  | If (var, et, ee) -> let set = substVariableName newName name et in let see = substVariableName newName name ee in
    if var = name then If(newName,set,see) else If(var,set,see)
  | Operation(e1, op, e2) -> Operation ((substVariableName newName name e1), op, (substVariableName newName name e2))
  | Negation e -> Negation (substVariableName newName name e)
  | New (cn, varList) -> let substVars = List.map varList ~f:(fun x -> if x = name then newName else x) in
    New (cn,substVars)
  | While (var, e) -> let se = substVariableName newName name e in
    if var = name then While (newName, se) else While (var,se)
  | Cast (cn, var) -> if var = name then Cast (cn,newName) else exp
  | InstanceOf (var, cn) -> if var = name then InstanceOf (newName, cn) else exp
  | MethodCall (var, mn, params) -> let substVar = (if var = name then newName else var) in
    let substParams = List.map params ~f:(fun p -> if p = name then newName else p) in MethodCall(substVar,mn,substParams)
  | Ret (v, e) -> exp (* TODO: Think twice about this case. Do we need to substitute also in this type of exp?*)
