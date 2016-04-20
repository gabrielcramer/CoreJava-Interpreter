open Heap
open Syntax

type state =
  { e: exp;
    heap: Heap.t;
    env: Syntax.typeValue Environment.t;
    prog: program;
  }

exception TypeError of string
exception RuntimeError of string


let isLocation = function
  | LocV(_) -> true
  | _ -> false

let rec findMainMethodRec (classDeclList : classDeclaration list) : exp = match classDeclList with
  (* TODO Raise Runtime Error if there is no main method *)
  | [] -> raise (RuntimeError "There is no main method.\n")
  | [Class(_, _, _, methodDeclaration)] -> (match methodDeclaration with
      | [MainMethod(_, _, exp)] -> exp
      | _ -> raise (RuntimeError "There is no main method.\n")
    )
  | Class(_, _, _, _) :: tl -> findMainMethodRec tl

let rec findMainMethod (program : program) : exp = match program with
    Program classDeclList -> findMainMethodRec classDeclList


(* let applyOp (bop: binaryOperator) (v1:value) (v2:value) :value = match bop with
    IPlus -> (match v1,v2 with
        IntV int1,IntV int2 -> IntV(int1 + int2))
   | IMinus -> ( match v1,v2 with
        IntV(int1),IntV(int2) -> IntV(int1 - int2))
   | IMultiply -> (match v1, v2 with
        IntV(int1), IntV(int2) -> IntV(int1 * int2))
   | IDivide -> (match v1, v2 with
        IntV(int1), IntV(int2) -> IntV(int1 / int2))
   (* TODO all the bops *)
   |_ -> NullV;; *)


(*let rec eval (e:exp) (h:heap) (env:stack) (prog:program) : value = match e with
    Value(v) -> v;
  | Variable(id) -> (try Environment.lookup id env with Environment.Not_bound -> raise(RuntimeError (id ^ "not declared."))
   | ObjectField(var,field) -> let loc = lookup var env in if isLocation loc then let fldE = getFieldEnv loc h in let v = lookup field fldE in v
    else raise(RuntimeError "Not a location");
  | Operation(exp1, op, exp2) -> applyOp op (eval exp1 h env prog) (eval exp2 h env prog); *)



let rec step (state : state) : state = match state.e with
  | Value _ -> Utils.rerr ("Does not step") (*This case is unreachable if step is called from multistep*)
  | Variable(id) -> if Environment.isIn id state.env then
      let v = (Environment.lookup id state.env).value in
      {state with e = Value(v)}
    else raise (RuntimeError (id ^ "not declared."))

  | ObjectField(var, field) -> stepObjectField var field state
  | VariableAssignment(id, exp) -> stepVariableAssignment id exp state
  | ObjectFieldAssignment((c, f), e) -> state (*TODO*)
  | Sequence(e1, e2) -> stepSequence e1 e2 state
  | BlockExpression(list, exp) -> stepBlockExpression list exp state
  | Ret(v, exp) -> stepRet v exp state
  | If (id, et, ee) -> state (*TODO*)
  | Operation(e1, op, e2) -> state (*TODO*)
  | Negation(e) -> state (*TODO*)
  | New(id, idList) -> stepNew id idList state
  | While(c, e) -> state (*TODO*)
  | Cast(t, id) -> state (*TODO*)
  | InstanceOf(id, t) -> state (*TODO*)
  | MethodCall(cn, mn, params) -> state (*TODO*)

and
  stepObjectField (var : id) (field : id) (state : state) : state = if Environment.isIn var state.env then
    let loc = (Environment.lookup var state.env).value in
    (* TODO: check if location is in heap *)
    if isLocation loc then let fldE = (Heap.getFieldEnv loc state.heap) in
      print_endline ((Utils.stringOfEnv fldE));
      if Environment.isIn field fldE then
        let v = (Environment.lookup field fldE).value in
        {state with e = Value(v)}
      else raise (RuntimeError("Field " ^ field ^ " not declared inside " ^ var))
    else Utils.rerr (var ^ " is not an object.")
  else raise (RuntimeError (var ^ "not declared."))

and
  stepVariableAssignment (id : id) (e : exp) (state : state) : state = match e with
  | Value(v) -> if Environment.isIn id state.env then
      let tVar = Utils.getTypeVar id state.env in let tVal = Utils.getTypeVal v in
      if Utils.isSubtype tVal tVar state.prog then
        let nEnv = Environment.update id {typ = tVar; value = v} state.env in
        {state with env = nEnv; e = Value(VoidV)}
      else Utils.rerr ("Invalid types")
    else Utils.rerr ("Unbound value " ^ id)
  | _ -> let ns = (step {state with e = e}) in {ns with e = VariableAssignment(id, ns.e)}

and
  stepSequence (e1 : exp) (e2 : exp) (state : state) : state = match e1 with
  | Value(v) -> { state with e = e2 }
  | _ -> let ns = (step {state with e = e1}) in {ns with e = Sequence(ns.e, e2)}

and
  stepBlockExpression (l : ((id * typ) list)) (exp : exp) (state : state) : state = match l with
  | [] -> { state with e = exp }
  | [(id,typ)] -> {state with
                   env = (Environment.extend id {typ = typ; value = Utils.initValue typ} state.env);
                   e = Ret(id, exp)}
  | (id,typ) :: tl -> {state with
                       env = (Environment.extend id {typ = typ; value = Utils.initValue typ} state.env);
                       e = Ret(id, BlockExpression(tl, exp))}

and
  stepRet (v : id) (exp : exp) (state : state) : state = if Utils.isValue exp then
    {state with env = (Environment.pop v state.env); e = exp}
  else
    let ns = (step {state with e = exp }) in {ns with e = Ret(v, ns.e)}

and
  stepNew (id : id) (idList : id list) (state : state) : state = match (Utils.firstUnboundVariable idList state.env) with
  | None -> (if Utils.definedInProg id state.prog then
               let fieldList = (Utils.getFieldList (ObjectType id) state.prog) in
               let typeList = Utils.getTypeList idList state.env in
               match (Utils.checkFieldsTypes fieldList typeList state.prog) with
               | None -> (let fEnv = (Utils.createFieldEnv fieldList idList state.env) in
                          let nl = LocV(666) in
                          let nh = Heap.extend nl {id = id; env = fEnv} state.heap in

                          {state with heap = nh; e = Value(nl)})
               | Some(field) -> Utils.rerr ("Field" ^ field ^ "uncompatibile with its corresponding value")
             else Utils.rerr (id ^ " not defined in prog"))
  | Some(var) -> Utils.rerr ("Unbound value " ^ var)


let rec multistep (state : state) : value = match state.e with
  | Value(v) -> v
  | exp -> print_endline ((Utils.stringOfEnv state.env)); multistep (step state)

let interpret (e : exp) (program : program) : value =
  let initialEnv = (Environment.union [
      ("a", {typ = IntType; value = IntV 3});
      ("mya",{typ = ObjectType("a"); value = NullV});
      ("myb",{typ = ObjectType("b"); value = NullV})] Environment.empty) in
  let initialState = {heap = Heap.empty; env = initialEnv; e = e; prog = program} in
  multistep initialState

let prg = Program( [Class ("a", "Object", [("f1",IntType)], []);
                    Class ("b", "a", [("f2",IntType)], []);Class ("c", "b", [], []);Class ("d", "Object", [], [])] )
(* Tests for Typechecker *)
let te = (Environment.union [("a",IntType);("cond",BoolType);("mya",ObjectType("a"));("myc",ObjectType("c"));("myd",ObjectType("d"))] Environment.empty)
let _ = assert (true = Utils.isSubtype VoidType VoidType prg)
let _ = assert (true = Utils.isSubtype (ObjectType "b") (ObjectType "Object") prg)
let _ = assert (true = Utils.isSubtype (ObjectType "c") (ObjectType "Object") prg)
let _ = assert (true = Utils.isSubtype (ObjectType "c") (ObjectType "a") prg)
let _ = assert (false = Utils.isSubtype (ObjectType "b") (ObjectType "d") prg)
let _ = assert (false = Utils.isSubtype (ObjectType "d") (ObjectType "b") prg)
let _ = assert ((ObjectType "Object") = Utils.getParent (ObjectType "a") prg)
let _ = assert (Some (ObjectType "Object") = Utils.leastMaxType (ObjectType "c") (ObjectType "d") prg )
let _ = assert (Some (ObjectType "a") = Utils.leastMaxType (ObjectType "c") (ObjectType "a") prg )

let _ = assert (IntType = Typechecker.typeCheckExp (Value(IntV 3)) te prg )
let _ = assert (IntType = Typechecker.typeCheckExp (ObjectField("mya", "f1")) te prg)
let _ = assert (VoidType = Typechecker.typeCheckExp (VariableAssignment ("a", Value(IntV 3)) ) te prg)
let _ = assert (ObjectType "a" = Typechecker.typeCheckExp (Variable "mya") te prg)
let _ = assert (ObjectType "Object" = Typechecker.typeCheckExp (If ("cond", (Variable "myc"), (Variable "myd")) ) te prg)

let _ = assert (IntType = Typechecker.typeCheckExp (Operation ((Value(IntV 3)),IPlus,(Value(IntV 3)))) te prg)
let _ = assert (BoolType = Typechecker.typeCheckExp (Operation ((Value(IntV 3)),EqEqual,(Value(IntV 3)))) te prg)
let _ = assert (true = Utils.isIntOperator IPlus)
let _ = assert (ObjectType "a" = Typechecker.typeCheckExp (Cast("a","myc")) te prg)
let _ = assert (ObjectType "Object" = Typechecker.typeCheckExp (Cast("Object","myc")) te prg)
let _ = assert (ObjectType "a" = Typechecker.typeCheckExp (New("a",["a"])) te prg)
let _ = assert (ObjectType "b" = Typechecker.typeCheckExp (New("b",["a";"a"])) te prg)
let _ = assert (VoidType = Typechecker.typeCheckExp (While ("cond",(Value(IntV 3))) ) te prg)
(* Tests for interpreter*)
(* let _ = assert (IntV 21 = interpret (Sequence(Value(IntV 22),Value(IntV 21) )) prg )
   let _ = assert (IntV 22 = interpret (Sequence(VariableAssignment("a",Value(IntV 22)),Variable("a") )) prg )
   let _ = assert (IntV 666 = interpret (Sequence(VariableAssignment("a",Sequence(Value(IntV 22),Value(IntV 666) )),Variable("a") )) prg )
   let _ = assert (IntV 0 = interpret (BlockExpression([(IntType,"b")], Variable("b") )) prg )
   let _ = assert (IntV 22 = interpret (BlockExpression([(IntType,"b")], Sequence(VariableAssignment("b",Value(IntV 22)),Variable("b") )))  prg ) *)
let _ = assert (LocV 666 = interpret (Sequence(VariableAssignment("mya", New("a", ["a"])), Variable("mya"))) prg)
let _ = assert (IntV 3 = interpret (Sequence(VariableAssignment("mya", New("a", ["a"])), ObjectField("mya", "f1"))) prg)
let _ = assert (IntV 3 = interpret (Sequence(VariableAssignment("myb", New("b", ["a";"a"])), ObjectField("myb", "f1"))) prg)
let _ = assert (IntV 3 = interpret (Variable "a") prg )
