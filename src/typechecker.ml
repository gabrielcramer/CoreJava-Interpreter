open Syntax
open Core.Std

let rec typeCheckExp exp tenv prog = match exp  with
  | Value(v) -> typeCheckValueExp v
  | Variable(id) -> typeCheckVariableExp id tenv
  | ObjectField(var, field) -> typeCheckObjectFieldExp var field tenv prog
  | VariableAssignment(id, exp) -> typeCheckVariableAssignmentExp id exp tenv prog
  | ObjectFieldAssignment((var, f), e) -> typeCheckObjectFieldAssignmentExp var f e tenv prog
  | Sequence(e1, e2) -> typeCheckSequenceExp e1 e2 tenv prog
  | BlockExpression(list, exp) -> typeCheckBlockExp list exp tenv prog
  | If (id, et, ee) -> typeCheckIfExp id et ee tenv prog
  | Operation(e1, op, e2) -> typeCheckOperationExp e1 e2 op tenv prog
  | Negation(e) -> typeCheckNegationExp e tenv prog
  | New(cn, varList) -> typeCheckNewExp cn varList tenv prog
  | While(var, e) -> typeCheckWhileExp var e tenv prog
  | Cast(cn, var) -> typeCheckCastExp cn var tenv prog
  | InstanceOf(var, cn) -> typeCheckInstanceOfExp var cn tenv prog
  | MethodCall(cn, mn, params) -> IntType (*TODO*)
  | Ret(v, exp) -> Utils.rerr ("Expression 'Ret' should not occur while type checking.")

and typeCheckValueExp = function
  | IntV _ -> IntType
  | FloatV _ -> FloatType
  | BoolV _ -> BoolType
  | VoidV -> VoidType
  | NullV -> NullType
  | LocV _ -> LocType

and typeCheckClassName cn prog = if Utils.definedInProg cn prog then (ObjectType cn)
 else Utils.rerr ("Unbound class " ^ cn)

and typeCheckVariableExp id tenv = if Environment.isIn id tenv then Environment.lookup id tenv else Raise_error.unboundVar id

and typeCheckObjectFieldExp var field tenv prog = let varType = typeCheckVariableExp var tenv in
  if Utils.isObjectType varType then let fieldType = Utils.getTypeField varType field prog  in
    if Option.is_some fieldType then Option.value_exn fieldType
    else Utils.rerr ("Field " ^ field ^ " not declared inside " ^ var)
  else Utils.rerr("Variable " ^ var ^ " is not an object." )


and typeCheckVariableAssignmentExp id exp tenv prog = let varType = typeCheckVariableExp id tenv in
  let expType = typeCheckExp exp tenv prog in if Utils.isSubtype expType varType prog then VoidType
  else Utils.rerr ("Type of " ^ id ^ "("^ (Utils.stringOfType varType) ^ ") is incompatible with " ^ (Utils.stringOfType expType))

and typeCheckObjectFieldAssignmentExp var field exp tenv prog = let fieldType = typeCheckObjectFieldExp var field tenv prog in
  let expType = typeCheckExp exp tenv prog in if Utils.isSubtype expType fieldType prog then VoidType
  else Utils.rerr ("Type of " ^ var ^ "."^field ^ "("^ (Utils.stringOfType fieldType) ^ ") is incompatible with " ^ (Utils.stringOfType expType))

and typeCheckSequenceExp e1 e2 tenv prog = let _ = typeCheckExp e1 tenv prog in typeCheckExp e2 tenv prog

and typeCheckBlockExp list exp tenv prog =  let newTE = (Environment.union list tenv) in typeCheckExp exp newTE prog

and typeCheckIfExp var et ee tenv prog  =  let varType =  typeCheckVariableExp var tenv in if Utils.isSubtype varType BoolType prog then
    let ett = typeCheckExp et tenv prog in let eet = typeCheckExp ee tenv prog in let lmt = Utils.leastMaxType ett eet prog in
    match lmt with Some t -> t
                 | None -> Utils.rerr ("Type " ^ (Utils.stringOfType ett) ^ " is not compatible with type " ^ (Utils.stringOfType eet))
  else Utils.rerr ("Variable " ^ var ^ " has type " ^ (Utils.stringOfType varType) ^ " but a variable was expected of type " ^ (Utils.stringOfType BoolType) )

and typeCheckOperationExp e1 e2 op tenv prog = if Utils.isIntOperator op then typeCheckSpecOperation e1 e2 IntType tenv prog
  else if Utils.isFloatOperator op then typeCheckSpecOperation e1 e2 FloatType tenv prog
  else if Utils.isBoolOperator op then typeCheckSpecOperation e1 e2 BoolType tenv prog
  else (*op is compOperator *)  typeCheckCompOperation e1 e2 tenv prog

and typeCheckSpecOperation e1 e2 typ tenv prog = let e1t = (typeCheckExp e1 tenv prog) in
  if Utils.isSubtype e1t typ prog then let e2t = (typeCheckExp e2 tenv prog) in if Utils.isSubtype e2t typ prog then
      typ else Utils.raiseDifferentTypeExpErr e2 e2t typ
  else Utils.raiseDifferentTypeExpErr e1 e1t typ

(* TODO: reconsider type checking for this case *)
and typeCheckCompOperation e1 e2 tenv prog = let e1t = (typeCheckExp e1 tenv prog) in
  let e2t = (typeCheckExp e2 tenv prog) in if ((Utils.isSubtype e1t e2t prog) && (Utils.isSubtype e2t e1t prog)
                                               && (not (Utils.isObjectType e1t)) && (not (Utils.isObjectType e2t) )) then BoolType
  else Utils.rerr ("Can not compare an expression of type "^ (Utils.stringOfType e1t) ^"with an expression of type " ^ (Utils.stringOfType e2t))

and typeCheckNegationExp e tenv prog = let et = (typeCheckExp e tenv prog) in if Utils.isSubtype et BoolType prog then BoolType
  else Utils.raiseDifferentTypeExpErr e et BoolType

and typeCheckCastExp cn var tenv prog = let cnType = (typeCheckClassName cn prog) in let varType = (typeCheckVariableExp var tenv) in
    if ((Utils.isSubtype cnType varType prog) || (Utils.isSubtype varType cnType prog ) ) then cnType
    else Utils.rerr ("Can not cast a variable of type " ^ (Utils.stringOfType varType) ^ " to type " ^ (Utils.stringOfType cnType) )

and typeCheckInstanceOfExp var cn tenv prog = let _ = (typeCheckClassName cn prog) in
 let _ = (typeCheckVariableExp var tenv) in BoolType


and typeCheckNewExp cn varList tenv prog = let cnType = (typeCheckClassName cn prog) in
 let fieldList = Utils.getFieldList cnType prog in let varTypes = (List.map varList (fun var -> typeCheckVariableExp var tenv)) in
 try if List.for_all2_exn fieldList varTypes (fun f vt -> match f with (fn,ft) -> (Utils.isSubtype vt ft prog)) then cnType else raise (Invalid_argument ".") with Invalid_argument _ ->  Utils.rerr ("Number of arguments is not equal with the number of fields")

and typeCheckWhileExp var e tenv prog =  let varType =  (typeCheckVariableExp var tenv) in
  if Utils.isSubtype varType BoolType prog then let _ = (typeCheckExp e tenv prog) in VoidType
  else Utils.raiseDifferentTypeExpErr (Variable var) varType BoolType
