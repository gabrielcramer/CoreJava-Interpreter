open Syntax
open Utils
open Exn2
open Core.Std
open StringTupleSet


let rec secureTCProgram program =
  try
    wellFoundedClasses program;
    let Program classList = program in
    List.iter classList (fun x -> Utils.methodsOnce_exn x);
    List.iter classList (fun x -> Utils.fieldsOnce_exn x);
    List.iter classList (fun x -> Utils.goodInheritance x program);
    List.iter classList (fun x -> wellTypedClass x program)
  with
  | DuplicateField (fn, tp) -> raise (StaticError ("Field " ^ fn ^ " is declared more than once"))
  | DuplicateMethod Method(mt, mn, _, _, _) -> raise (StaticError ("Method " ^ mn ^ " is declared more than once"))
  | BadMethodOverriding Method(mt, mn, _, _, _) -> raise (StaticError ("Overriding method  " ^ mn ^ " is not sound"))
  | BadTypedMethod Method(mt, mn, _, _, _) -> raise (StaticError ("Method " ^ mn ^ " is not well typed."))
  | BadFoundedClassesError msg -> raise (StaticError msg)

and wellFoundedClasses prog = let Program classList = prog in
  let ir = List.fold classList ~init:StringTupleSet.empty
      ~f:(fun acc cl -> let Class(c, _, p, _, _) = cl in StringTupleSet.add acc (c, p)) in
  let id = List.fold classList ~init:StringTupleSet.empty
      ~f:(fun acc cl -> let Class(c, _, _, _, _) = cl in StringTupleSet.add acc (c, c)) in
  let ir = transitiveClosure ir in
  let inter = StringTupleSet.inter ir id in
  if StringTupleSet.length inter = 0 then
    let classNames = List.map classList ~f:(function Class(class_name, _, _ ,_ , _) -> class_name) in
    let dup = List.find_a_dup classNames in
    match dup with
    | None -> checkLastClass classList
    | Some d -> raise (BadFoundedClassesError ("Redefinition of class " ^ d))
  else
    raise (BadFoundedClassesError ("There is a cycle in the class hierarchy"))

and checkLastClass classList = match List.last classList with
  | None -> ()
  | Some Class(n, _, _, _, methods) -> if n = "Main" then begin
      match List.last methods with
      | None -> raise (BadFoundedClassesError "The main class has no methods.")
      | Some Method(mt, mn, args, lb, e) -> if mn = "main" then
          if mt = {typ= VoidType;label= L} then ()
          else raise (BadFoundedClassesError ("The main method has " ^ Utils.stringOfSecureType mt
                                              ^ " return type but was expected of return type " ^ Utils.stringOfType VoidType))
        else raise (BadFoundedClassesError "There is no main method inside Main class.")
    end
    else
      raise (BadFoundedClassesError ("Name of the last class has to be \"Main\"." ^ n))

and transitiveClosure ir =
  let newIR = StringTupleSet.fold ir ~init:ir ~f:(fun acc pair ->
      match pair with (cn1, cn2) ->
        let newPairs = StringTupleSet.fold ir ~init:StringTupleSet.empty ~f:(fun acc2 pair ->
            match pair with (cn3, cn4) -> if cn2 = cn3 then StringTupleSet.add acc2 (cn1, cn4) else acc2) in
        StringTupleSet.union newPairs acc) in
  if StringTupleSet.equal ir newIR then newIR
  else transitiveClosure newIR

and wellTypedClass c prog = let Class(cn, cl, _, _, methods) = c in
  let stenv = Environment.extend "this" {typ= (ObjectType cn); label= cl }
      Environment.empty in List.iter methods ~f:(fun m -> if wellTypedMethod m stenv prog then ()
                                                  else raise (BadTypedMethod m))

and wellTypedMethod m stenv prog  = let Method(mt, _, args, ml, exp) = m in
  let newSTE = (Environment.union args stenv) in
  let expType = secureTCExp exp newSTE ml prog in
  isSecureSubtype expType mt prog

and secureTCExp exp stenv hl prog = match exp with
  | Value(v) -> {typ = Utils.getTypeOfVal v; label=L}
  | Variable(var) -> secureTCVariable var stenv
  | ObjectField(var, field) -> secureTCObjectField var field stenv prog (*TODO*)
  | VariableAssignment (var, exp)-> secureTCVariableAssignment var exp stenv hl prog
  | ObjectFieldAssignment((var, field), e) -> secureTCObjectFieldAssignment var field e stenv hl prog
  | Sequence(e1, e2) -> secureTCSequence e1 e2 stenv hl prog
  | BlockExpression(list, exp) -> secureTCBlock list exp stenv hl prog
  | If (var, et, ee) -> secureTCIf var et ee stenv hl prog
  | Operation(e1, op, e2) -> secureTCOperation e1 e2 op stenv hl prog
  | Negation(e) -> secureTCNegation e stenv hl prog
  | New(cn, varList) -> secureTCNew cn varList stenv hl prog
  | While(var, e) -> secureTCWhile var e stenv hl prog
  | Cast(cn, var) -> secureTCCast cn var stenv hl prog
  | InstanceOf(var, cn) -> secureTCInstanceOf var cn stenv hl prog
  | MethodCall(var, mn, params) -> secureTCMethodCall var mn params stenv hl prog
  | Ret(v, exp) -> raiseStaticError ("Expression 'Ret' should not occur while type checking.")

and secureTCClassName cn prog = match getClassLabel cn prog with
  | Some l -> {typ= ObjectType cn; label= l}
  | None -> raiseStaticError ("Unbound class " ^ cn)

and secureTCVariable var stenv = if Environment.isIn var stenv then
    Environment.lookup var stenv
  else raiseUnboundVar var
and secureTCObjectAndField var field stenv prog = let varType = secureTCVariable var stenv in
  if Utils.isObjectType varType.typ then let fieldType = Utils.getTypeField varType.typ field prog in
    if Option.is_some fieldType then let fieldType = Option.value_exn fieldType in
      (varType, fieldType)
    else raiseStaticError ("Field " ^ field ^ " not declared inside " ^ var)
  else raiseStaticError ("Variable " ^ var ^ " is not an object." )

and secureTCObjectField var field stenv prog = let (varType, fieldType) = secureTCObjectAndField var field stenv prog in
  {fieldType with label= lubLabel varType.label fieldType.label}

and secureTCVariableAssignment var exp stenv hl prog = if var <> "this" then let varType = secureTCVariable var stenv in
    let expType = secureTCExp exp stenv hl prog in if isSecureSubtype expType varType prog then {varType with typ = VoidType}
    else raiseStaticError ("Insecure information flow from " ^ (Utils.stringOfExp exp) ^ " to " ^ var)
  else raiseStaticError ("Cannot assign a value to final variable this.")

and  secureTCObjectFieldAssignment var field exp stenv hl prog = let varType,fieldType = secureTCObjectAndField var field stenv prog in
  let expType = secureTCExp exp stenv hl prog in
  if isSecureSubtype expType fieldType prog && isSubLabel hl fieldType.label then
    {typ = VoidType; label = L}
  else raiseStaticError ("Insecure information flow from " ^ (Utils.stringOfExp exp) ^ " to " ^ (Utils.stringOfExp (ObjectField (var, field))))

and secureTCSequence e1 e2 stenv hl prog = let e1Type = secureTCExp e1 stenv hl prog in
  let e2Type = secureTCExp e2 stenv hl prog in {e2Type with label= glbLabel e1Type.label e2Type.label}

and secureTCBlock list exp stenv hl prog = let newSTE = Environment.union list stenv in
  secureTCExp exp newSTE hl prog

and secureTCIf var et ee stenv hl prog =  let varType = secureTCVariable var stenv in
  if isSubtype varType.typ BoolType prog then
    let etType = secureTCExp et stenv hl prog in
    let eeType = secureTCExp ee stenv hl prog in
    if isSubLabel varType.label hl &&
       isSubLabel varType.label etType.label &&
       isSubLabel varType.label eeType.label then
      let lmt = Utils.leastMaxType etType.typ eeType.typ prog in
      match lmt with Some t -> {typ= t; label = glbLabel etType.label eeType.label}
                   | None -> raiseStaticError ("Type " ^ (Utils.stringOfType etType.typ) ^ " is not compatible with type " ^ (Utils.stringOfType eeType.typ))

    else let _ = print_endline (var ^ " type:" ^ (stringOfSecureType varType) ^
                                "  etType:" ^ (stringOfSecureType etType) ^
                                "  eeType:" ^ (stringOfSecureType eeType)
                               ) in raiseStaticError ("Insecure information flow inside expression " ^ (Utils.stringOfExp @@ If (var, et, ee) ))
  else raiseStaticError ("Variable " ^ var ^ " has type " ^ (Utils.stringOfType varType.typ) ^ " but a variable was expected of type " ^ (Utils.stringOfType BoolType) )

and secureTCOperation e1 e2 op stenv hl prog = match op with
  | op when Utils.isBoolOperator op -> secureTCSpecificOp e1 e2 BoolType stenv hl prog
  | op when Utils.isIntOperator op -> secureTCSpecificOp e1 e2 IntType stenv hl prog
  | op when Utils.isFloatOperator op -> secureTCSpecificOp e1 e2 FloatType stenv hl prog
  | op (*compare operator*) -> secureTCCompare e1 e2 stenv hl prog

and secureTCSpecificOp e1 e2 typ stenv hl prog = let e1Type = secureTCExp e1 stenv hl prog in
  if isSubtype e1Type.typ typ prog then
    let e2Type = secureTCExp e2 stenv hl prog in
    if isSubtype e2Type.typ typ prog then
      {typ= typ; label = lubLabel e1Type.label e2Type.label}
    else raiseStaticError ("Expression " ^ (Syntax.show_exp e2) ^
                           " has type " ^ (Syntax.show_typ e2Type.typ) ^
                           " but an expression was expected of type " ^
                           (stringOfType typ))
  else raiseStaticError ("Expression " ^ (Syntax.show_exp e1) ^
                         " has type " ^ (Syntax.show_typ e1Type.typ) ^
                         " but an expression was expected of type " ^
                         (stringOfType typ))


and secureTCCompare e1 e2 stenv hl prog =  let e1Type = secureTCExp e1 stenv hl prog in
  let e2Type = secureTCExp e2 stenv hl prog in
  if e1Type.typ = e2Type.typ && not(isObjectType e1Type.typ) then
    {typ= BoolType; label= lubLabel e1Type.label e2Type.label}

  else
    raiseStaticError (sprintf "Can not compare an expression of type `%s` with an expression of type `%s`"
                        (Utils.stringOfType e1Type.typ)
                        (Utils.stringOfType e2Type.typ))

and secureTCNegation exp stenv hl prog = let expType = secureTCExp exp stenv hl prog in
  if isSubtype expType.typ BoolType prog then expType
  else raiseStaticError ("Expression " ^ (Syntax.show_exp exp) ^
                         " has type " ^ (Syntax.show_typ expType.typ) ^
                         " but an expression was expected of type " ^
                         (stringOfType BoolType))

and secureTCNew cn varList stenv hl prog = let objType = secureTCClassName cn prog in
  let fieldList = getFieldList objType.typ prog in
  let varTypes = List.map varList ~f:(fun x -> secureTCVariable x stenv) in
  try
    if List.for_all2_exn fieldList varTypes
        ~f:(fun f vt -> match f with (fn, ft) ->
            isSecureSubtype vt ft prog) then
      if isSubLabel hl objType.label then
        objType
      else raiseStaticError ("Cannot have " ^(Syntax.show_label objType.label) ^ " effect on the heap inside a context of level " ^ (Syntax.show_label hl) ^ " in expresion: " ^ (Syntax.show_exp (New (cn,varList))) )
    else raiseStaticError ("Type of the parameters are not compatible with the fields of class " ^ cn)
  with
  | Invalid_argument _-> raiseStaticError "Number of arguments is not equal with the number of fields"


and secureTCWhile var exp stenv hl prog = let varType = secureTCVariable var stenv in
  if isSubtype varType.typ BoolType prog then
    let expType = secureTCExp exp stenv hl prog in
    if isSubLabel varType.label hl &&
       isSubLabel varType.label expType.label then
      {typ= VoidType; label= varType.label}
    else raiseStaticError ("Insecure flow of information in expression: " ^
                           (stringOfExp (While (var,exp))) )
  else raiseStaticError ("Variable " ^ var ^ " has type " ^
                         (Utils.stringOfType varType.typ) ^
                         " but a variable was expected of type " ^
                         (Utils.stringOfType BoolType) )

and secureTCCast cn var stenv hl prog = let cnType = secureTCClassName cn prog in
  let varType = secureTCVariable var stenv in
  if isSubtype cnType.typ varType.typ prog then
    {cnType with label = varType.label}
  else
    raiseStaticError (sprintf "Can not cast variable `%s` of type `%s` to type `%s`"
                        var (stringOfType varType.typ) (Utils.stringOfType cnType.typ))



and secureTCInstanceOf var cn stenv hl prog = let _ = secureTCClassName cn prog in
  let varType = secureTCVariable var stenv in
  {typ=BoolType; label= varType.label}


and secureTCMethodCall var mn params stenv hl prog = let varType = secureTCVariable var stenv in
  if isTypeDeclared varType.typ prog then
    let methodDecl = Utils.getMethodDefinition varType.typ mn prog in
    if Option.is_some methodDecl then
      let Method(rt, _, idTypList, ml, _) = Option.value_exn methodDecl in
      let paramTypes = List.map params ~f:(fun x -> secureTCVariable x stenv) in
      try
        let validParams = List.for_all2_exn paramTypes idTypList
            (fun paramType -> function (_, typ) -> Utils.isSecureSubtype paramType typ prog) in
        if validParams then
          if isSubLabel hl ml then
            {rt with label= lubLabel rt.label varType.label}
          else
            raiseStaticError "Cannot call a low method inside a high context."
        else
          raiseStaticError "Types of the parameters are not valid."

      with
      | Invalid_argument _ -> raiseStaticError "Number of passed parameters is not valid."
    else raiseStaticError (sprintf "Method `%s` is not defined inside" (Utils.stringOfSecureType varType))
  else raiseStaticError ((Utils.stringOfSecureType varType) ^ " not declared inside program.")
