open Syntax
open Core.Std
open StringTupleSet
open Exn2

let rec typeCheckProgram program =
  try
    wellFoundedClasses program;
    let Program classList = program in
    List.iter classList (fun x -> Utils.methodsOnce_exn x);
    List.iter classList (fun x -> Utils.fieldsOnce_exn x);
    List.iter classList (fun x -> Utils.goodInheritance x program);
    List.iter classList (fun x -> wellTypedClass x program)
  with
  | DuplicateField (fn, tp) -> raise (StaticError ("Field " ^ fn ^ " is declared more than once"))
  | DuplicateMethod Method(mt,mn,_,_) -> raise (StaticError ("Method " ^ mn ^ " is declared more than once"))
  | BadMethodOverriding Method(mt,mn,_,_) -> raise (StaticError ("Overriding method  " ^ mn ^ " is not sound"))
  | BadTypedMethod Method(mt,mn,_,_) -> raise (StaticError ("Method " ^ mn ^ " is not well typed."))
  | BadFoundedClassesError msg -> raise (StaticError msg)


and wellFoundedClasses prog = let Program classList = prog in
  let ir = List.fold classList ~init:StringTupleSet.empty
      ~f:(fun acc cl -> let Class(c, p, _, _) = cl in StringTupleSet.add acc (c, p)) in
  let id = List.fold classList ~init:StringTupleSet.empty
      ~f:(fun acc cl -> let Class(c, _, _, _) = cl in StringTupleSet.add acc (c, c)) in
  let ir = transitiveClosure ir in
  let inter = StringTupleSet.inter ir id in
  if StringTupleSet.length inter = 0 then
    let classNames = List.map classList ~f:(function Class(class_name, _ ,_ , _) -> class_name) in
    let dup = List.find_a_dup classNames in
    match dup with
    | None -> checkLastClass classList
    | Some d -> raise (BadFoundedClassesError ("Error: Redefinition of class " ^ d))
  else
    raise (BadFoundedClassesError ("Error: There is a cycle in the class hierarchy"))

and transitiveClosure ir =
  let newIR = StringTupleSet.fold ir ~init:ir ~f:(fun acc pair ->
      match pair with (cn1, cn2) ->
        let newPairs = StringTupleSet.fold ir ~init:StringTupleSet.empty ~f:(fun acc2 pair ->
            match pair with (cn3, cn4) -> if cn2 = cn3 then StringTupleSet.add acc2 (cn1, cn4) else acc2) in
        StringTupleSet.union newPairs acc) in
  if StringTupleSet.equal ir newIR then newIR
  else transitiveClosure newIR

and checkLastClass classList = match List.last classList with
  | None -> ()
  | Some Class(n, _, _, methods) -> if n = "Main" then begin
      match List.last methods with
      | None -> raise (BadFoundedClassesError "Error: The main class has no methods.")
      | Some Method(mt, mn, args, e) -> if mn = "main" then
          if mt = VoidType then ()
          else raise (BadFoundedClassesError ("Error: The main method has " ^ Utils.stringOfType mt
                                              ^ " return type but was expected of return type " ^ Utils.stringOfType VoidType))
        else raise (BadFoundedClassesError "Error: There is no main method inside Main class.")
    end
    else
      raise (BadFoundedClassesError ("Error: Name of the last class has to be \"Main\"." ^ n))


and wellTypedClass c prog = let Class(cn,_,_,methods) = c in
  let tenv = Environment.extend "this" (ObjectType cn) Environment.empty in
  List.iter methods (fun m -> if wellTypedMethod m tenv prog then () else raise(BadTypedMethod m))

and wellTypedMethod m tenv prog = let Method(mt,_,args,e) = m in
  let newTE = (Environment.union args tenv) in
  let et = typeCheckExp e newTE prog in
  Utils.isSubtype et mt prog

and typeCheckExp exp tenv prog = match exp with
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
  | MethodCall(cn, mn, params) -> typeCheckMethodCallExp cn mn params tenv prog
  | Ret(v, exp) -> raiseStaticError ("Expression 'Ret' should not occur while type checking.")

and typeCheckValueExp = function
  | IntV _ -> IntType
  | FloatV _ -> FloatType
  | BoolV _ -> BoolType
  | VoidV -> VoidType
  | NullV -> NullType
  | LocV _ -> LocType

and typeCheckClassName cn prog = if Utils.isDefinedInProg cn prog then (ObjectType cn)
  else raiseStaticError ("Unbound class " ^ cn)

and typeCheckVariableExp id tenv = if Environment.isIn id tenv then Environment.lookup id tenv else raiseUnboundVar id

and typeCheckObjectFieldExp var field tenv prog = let varType = typeCheckVariableExp var tenv in
  if Utils.isObjectType varType then
    let fieldType = Utils.getTypeField varType field prog in
    if Option.is_some fieldType then Option.value_exn fieldType
    else raiseStaticError ("Field " ^ field ^ " not declared inside " ^ var)
  else raiseStaticError ("Variable " ^ var ^ " is not an object." )

and typeCheckVariableAssignmentExp id exp tenv prog = let varType = typeCheckVariableExp id tenv in
  let expType = typeCheckExp exp tenv prog in
  if Utils.isSubtype expType varType prog then VoidType
  else raiseStaticError ("Type of " ^ id ^ "(" ^ (Utils.stringOfType varType) ^ ") is incompatible with " ^ (Utils.stringOfType expType))

and typeCheckObjectFieldAssignmentExp var field exp tenv prog = let fieldType = typeCheckObjectFieldExp var field tenv prog in
  let expType = typeCheckExp exp tenv prog in if Utils.isSubtype expType fieldType prog then VoidType
  else raiseStaticError ("Type of " ^ var ^ "." ^ field ^ "(" ^ (Utils.stringOfType fieldType) ^ ") is incompatible with " ^ (Utils.stringOfType expType))

and typeCheckSequenceExp e1 e2 tenv prog = let _ = typeCheckExp e1 tenv prog in typeCheckExp e2 tenv prog

and typeCheckBlockExp list exp tenv prog = let newTE = Environment.union list tenv in typeCheckExp exp newTE prog

and typeCheckIfExp var et ee tenv prog = let varType = typeCheckVariableExp var tenv in
  if Utils.isSubtype varType BoolType prog then
    let ett = typeCheckExp et tenv prog in
    let eet = typeCheckExp ee tenv prog in
    let lmt = Utils.leastMaxType ett eet prog in
    match lmt with Some t -> t
                 | None -> raiseStaticError ("Type " ^ (Utils.stringOfType ett) ^ " is not compatible with type " ^ (Utils.stringOfType eet))
  else raiseStaticError ("Variable " ^ var ^ " has type " ^ (Utils.stringOfType varType) ^ " but a variable was expected of type " ^ (Utils.stringOfType BoolType) )

and typeCheckOperationExp e1 e2 op tenv prog = if Utils.isIntOperator op then typeCheckSpecOperation e1 e2 IntType tenv prog
  else if Utils.isFloatOperator op then typeCheckSpecOperation e1 e2 FloatType tenv prog
  else if Utils.isBoolOperator op then typeCheckSpecOperation e1 e2 BoolType tenv prog
  else (* op is compOperator *)  typeCheckCompOperation e1 e2 tenv prog

and typeCheckSpecOperation e1 e2 typ tenv prog = let e1t = (typeCheckExp e1 tenv prog) in
  if Utils.isSubtype e1t typ prog then
    let e2t = (typeCheckExp e2 tenv prog) in
    if Utils.isSubtype e2t typ prog then typ
    else raiseDifferentTypeExpErr e2 [e2t] typ
  else raiseDifferentTypeExpErr e1 [e1t] typ

(* TODO: reconsider type checking for this case *)
and typeCheckCompOperation e1 e2 tenv prog = let e1t = (typeCheckExp e1 tenv prog) in
  let e2t = (typeCheckExp e2 tenv prog) in
  if (Utils.isSubtype e1t e2t prog) && (Utils.isSubtype e2t e1t prog)
     && (not (Utils.isObjectType e1t)) && (not (Utils.isObjectType e2t)) then BoolType
  else raiseStaticError (sprintf "Can not compare an expression of type `%s` with an expression of type `%s`" (Utils.stringOfType e1t) (Utils.stringOfType e2t))

and typeCheckNegationExp e tenv prog = let et = (typeCheckExp e tenv prog) in if Utils.isSubtype et BoolType prog then BoolType
  else raiseDifferentTypeExpErr e [et] BoolType

and typeCheckCastExp cn var tenv prog = let cnType = (typeCheckClassName cn prog) in let varType = (typeCheckVariableExp var tenv) in
  if (Utils.isSubtype cnType varType prog) || (Utils.isSubtype varType cnType prog ) then cnType
  else raiseStaticError (sprintf "Can not cast variable `%s` of type `%s` to type `%s`" var (Utils.stringOfType varType) (Utils.stringOfType cnType))

and typeCheckInstanceOfExp var cn tenv prog = let _ = (typeCheckClassName cn prog) in
  let _ = (typeCheckVariableExp var tenv) in BoolType

and typeCheckNewExp cn varList tenv prog = let cnType = (typeCheckClassName cn prog) in
  let fieldList = Utils.getFieldList cnType prog in
  let varTypes = (List.map varList (fun var -> typeCheckVariableExp var tenv)) in
  try
    if List.for_all2_exn fieldList varTypes (fun f vt -> match f with (fn, ft) -> (Utils.isSubtype vt ft prog)) then
      cnType
    else
      raise (Invalid_argument ".")
  with
    Invalid_argument _ -> raiseStaticError ("Number of arguments is not equal with the number of fields")

and typeCheckWhileExp var e tenv prog =  let varType =  (typeCheckVariableExp var tenv) in
  if Utils.isSubtype varType BoolType prog then
    let _ = (typeCheckExp e tenv prog) in
    VoidType
  else
    raiseDifferentTypeExpErr (Variable var) [varType] BoolType

and typeCheckMethodCallExp var mn params tenv prog = let varType = typeCheckVariableExp var tenv in
  if Utils.isTypeDeclared varType prog then
    let methodDecl =  Utils.getMethodDefinition varType mn prog in
    if Option.is_some methodDecl then
      let Method(rt, _, idTypLst, _) = Option.value_exn methodDecl in
      let paramTypes = List.map params (fun x -> typeCheckVariableExp x tenv)  in
      try
        let validParams = List.for_all2_exn paramTypes idTypLst
            (fun paramType -> function (_, typ) -> Utils.isSubtype paramType typ prog) in
        if validParams then rt else raiseStaticError "Types of the parameters are not valid."
      with
        Invalid_argument _ -> raiseStaticError "Number of passed parameters is not valid."
    else raiseStaticError (sprintf "Method `%s` is not defined inside" (Utils.stringOfType varType))
  else raiseStaticError ((Utils.stringOfType varType) ^ " not declared inside program.")


let rec secureTypeCheckExp exp stenv hl prog = match exp with
  | Value(v) -> {typ = IntType; label=L} (*TODO*)
  | Variable(var) -> secureTypeCheckVariableExp var stenv
  | ObjectField(var, field) -> secureTypeCheckObjectFieldExp var field stenv hl prog (*TODO*)
  | VariableAssignment (var, exp)-> secureTypeCheckVariableAssignmentExp var exp stenv hl prog
  | ObjectFieldAssignment((var, f), e) -> {typ = IntType; label=L} (*TODO*)
  | Sequence(e1, e2) -> {typ = IntType; label=L} (*TODO*)
  | BlockExpression(list, exp) -> {typ = IntType; label=L} (*TODO*)
  | If (var, et, ee) -> secureTypeCheckIfExp var et ee stenv L prog
  | Operation(e1, op, e2) -> {typ = IntType; label=L} (*TODO*)
  | Negation(e) -> {typ = IntType; label=L} (*TODO*)
  | New(cn, varList) -> {typ = IntType; label=L} (*TODO*)
  | While(var, e) -> {typ = IntType; label=L} (*TODO*)
  | Cast(cn, var) -> {typ = IntType; label=L} (*TODO*)
  | InstanceOf(var, cn) -> {typ = IntType; label=L} (*TODO*)
  | MethodCall(cn, mn, params) -> {typ = IntType; label=L} (*TODO*)
  | Ret(v, exp) -> raiseStaticError ("Expression 'Ret' should not occur while type checking.")

and secureTypeCheckVariableExp var stenv = if Environment.isIn var stenv then Environment.lookup var stenv else raiseUnboundVar var

and secureTypeCheckObjectFieldExp var fn stenv hl prog = let varType = secureTypeCheckVariableExp var stenv in print_endline ("INTERN"^ Utils.stringOfSecureType varType); if Utils.isObjectType varType.typ then
    let fTypeOption = Utils.getSecureTypeField varType.typ fn prog in if Option.is_some fTypeOption then let fType = Option.value_exn fTypeOption in
      {fType with label= Utils.lubLabel varType.label fType.label}
    else raiseStaticError ("Field " ^ fn ^ " not declared inside " ^ var)
  else raiseStaticError ("Variable " ^ var ^ " is not an object." )



and secureTypeCheckVariableAssignmentExp var exp stenv hl prog = let varType = secureTypeCheckVariableExp var stenv in
  let expType = secureTypeCheckExp exp stenv hl prog in if Utils.isSecureSubtype expType varType prog then {typ=VoidType; label=hl}
  else raiseStaticError ("Insecure information flow from " ^ (Utils.stringOfExp exp) ^ " to " ^ var)

and secureTypeCheckIfExp var et ee stenv cl prog = let varType = secureTypeCheckVariableExp var stenv in if Utils.isSubtype varType.typ BoolType prog then
    let ett = secureTypeCheckExp et stenv varType.label prog in let eet = secureTypeCheckExp et stenv varType.label prog in
    if ett.label = eet.label then let lmt = Utils.leastMaxType ett.typ eet.typ prog in
      match lmt with Some t -> {typ = t;label = ett.label}
                   | None -> raiseStaticError ("Type " ^ (Utils.stringOfType ett.typ) ^ " is not compatible with type " ^ (Utils.stringOfType eet.typ))
    else raiseStaticError ("Security level of " ^ (Utils.stringOfExp et) ^" is not equal to the security level of " ^ (Utils.stringOfExp ee))
  else raiseStaticError ("Variable " ^ var ^ " has type " ^ (Utils.stringOfType varType.typ) ^ " but a variable was expected of type " ^ (Utils.stringOfType BoolType) )
