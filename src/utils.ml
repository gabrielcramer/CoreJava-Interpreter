open Syntax
exception RuntimeError of string

let getTypeVar var (env:Syntax.typeValue Environment.t): Syntax.typ =
  (Environment.lookup var env).typ

let getTypeVal = function
  (* TODO: Think about Null and Void *)
    IntV _ -> IntType
  | FloatV _ -> FloatType
  | BoolV _ -> BoolType
  | VoidV -> VoidType
  | LocV _ -> LocType
  | NullV -> NullType

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

let rec definedInProgAux (id:Syntax.id) (classList:Syntax.classDeclaration list): bool = match classList with
    Class(c,_,_,_) :: tl -> if c = id then true else definedInProgAux id tl
  | [] -> false

let definedInProg id = function Program classList -> definedInProgAux id classList




let rec firstUnboundVariable params env = match params with
    id::tl -> if Environment.isIn id env then firstUnboundVariable tl env else Some(id)
  | [] -> None

let rec getParent cn = function Program classList -> getParentAux cn classList

and
  getParentAux cn = function
    Class(c,p,_,_) :: tl -> if c = cn then p else getParentAux cn tl
  | [] ->  raise(RuntimeError (cn^ " class not declared inside program."))


let rec getFieldList cn prog = match cn with
    "Object" -> []
  | _ ->  let p = (getParent cn prog) in (getFieldList p prog) @ (getFieldListAux1 cn prog)

and getFieldListAux1 cn = function Program classList -> getFieldListAux2 cn classList

and getFieldListAux2 (cn: Syntax.id) (classList: Syntax.classDeclaration list) = match classList with
    [Class(c,_,fields,_)] -> if c = cn then fields else []
  | Class(c,_,fields,_) :: tl -> if c = cn then fields else getFieldListAux2 cn tl
  | [] -> [] (*think about this case*)

let rec getTypeList idList env =
  List.map (fun id -> getTypeVar id env) idList

let rec checkFieldsTypes fields types = match fields, types with
    (tf,f)::tlf, tv::tlt -> if isSubtype tv tf then checkFieldsTypes tlf tlt
    else Some(f)
  | [],[] -> None
  | _ -> raise(RuntimeError ("Default case reached in Utils.checkFieldsTypes")) (*TODO better treat this case*)


let rec createFieldEnv (fields: (Syntax.typ* Syntax.id) list) idList (env: Syntax.typeValue Environment.t) = match fields, idList with
    (tf,f)::tlf, id::tl -> let v = (Environment.lookup id env) in
    Environment.extend f v (createFieldEnv tlf tl env)
  | [],[] -> Environment.empty
  | _ ->  raise(RuntimeError ("Default case reached in Utils.createFieldEnv")) (*TODO better treat this case*)


let stringOfType = function
    IntType -> "IntType"
  | FloatType -> "FloatType"
  | BoolType -> "BoolType"
  | VoidType -> "VoidType"
  | NullType -> "NullType"
  | LocType -> "LocType"
  | ObjectType(obj) -> "ObjectType( " ^ obj ^" )"

let stringOfValue = function
    NullV -> "NullV"
  |IntV(i) -> "Int " ^ (string_of_int i)
  |FloatV(f) -> "Float " ^ (string_of_float f)
  |BoolV(b) -> "Bool " ^ (string_of_bool b)
  |VoidV -> "Void"
  |LocV l -> "Location" ^ (string_of_int l)

let stringOfEnv env =
  let stringList = (Environment.map (fun id typeValue ->"(" ^ id ^ " {typ="^(stringOfType typeValue.typ)^ ";value="^(stringOfValue typeValue.value) ^"})" ) env) in (String.concat " , " stringList)
