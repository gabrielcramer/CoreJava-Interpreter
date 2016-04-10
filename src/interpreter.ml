open Syntax
(* type state = heap * stack * exp *)

type state =
  { e: exp;
    heap: Heap.t;
    env: Syntax.typeValue Environment.t;
    prog: program;
  }
exception TypeError of string
exception RuntimeError of string


(* (varname * value)list *)

(* provisory implementation *)
(* let rec lookup (id:id) (env:stack) : value = match env with
    [] -> raise (RuntimeError("Id not declared."))
   | (hd1, hd2)::tl -> if hd1 = id then let (_,value) = hd2 in value else lookup id tl *)

(* let rec isIn (id:id) (env:stack): bool = match env with
    [] -> false
   | (hd1, _)::tl -> if hd1 = id then true else isIn id tl
*)

(* let rec isInHeap (loc:int) (h:heap) : bool = match h with
    [] -> false
   | (hd1, _)::tl -> if hd1 = loc then true else isInHeap loc tl
*)

(* let rec getFieldEnv (location:value) (h:heap) : Syntax.value Environment.t = match h,location with
    [],_ -> raise (RuntimeError ("Location not in the heap"))
   | (hd1, hd2)::tl, LocV(loc) -> if hd1 = loc then hd2.env else getFieldEnv location tl *)


(* let rec getTypeVar (id: id) (env:stack): typ = match env with
    (hd1, hd2) :: tl -> if id = hd1 then match hd2 with
                                          (t,v) -> t
                                    else getTypeVar id tl
   | [] -> raise(RuntimeError("Unbound variable: " ^ id)) *)



let isLocation = function
  | LocV(_) -> true
  | _ -> false

let rec findMainMethodRec (classDeclList : classDeclaration list) : exp = match classDeclList with
  (* TODO Raise Runtime Error if there is no main method *)
    [] -> raise (RuntimeError "There is no main method.\n")
  | [Class(_, _, _, methodDeclaration)] -> (match methodDeclaration with
        [MainMethod(_, _, exp)] -> exp
      | _ -> raise (RuntimeError "There is no main method.\n")
    )
  | Class(_, _, _, _)::tl -> findMainMethodRec tl

let rec findMainMethod (program:program) :exp = match program with
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
let rerr msg = raise(RuntimeError msg)


let rec step (state:state): state = match state.e with
    Variable(id) -> if Environment.isIn id state.env then let v = (Environment.lookup id state.env).value in {state with e = Value(v)}
    else raise (RuntimeError (id ^ "not declared."))


  | ObjectField(var,field) -> if Environment.isIn var state.env
    then let loc = (Environment.lookup var state.env).value  in
      (* TODO: check if location is in heap *)
      if isLocation loc then let fldE = (Heap.getFieldEnv loc state.heap) in
        if Environment.isIn field fldE then let v = (Environment.lookup field fldE).value in {state with e = Value(v)}
        else raise (RuntimeError("Field:" ^ field ^ "not declared inside " ^ var))
      else rerr (var ^ "is not an object.")
    else raise (RuntimeError (var ^ "not declared."))
  | VariableAssignment(id, exp) -> stepVariableAssignment id exp state
  | Sequence(e1,e2) -> stepSequence e1 e2 state
  | BlockExpression(list, exp) -> stepBlockExpression list exp state
  | Ret(v,exp) -> stepRet v exp state


(* | VariableAssignment(id,exp) -> stepVariableAssignment id exp h env prog  *)
(*| Operation(exp1, op, exp2) -> applyOp op (eval exp1 h env prog) (eval exp2 h env prog);; *)

and
  stepVariableAssignment (id:id) (e:exp) (state:state):state = match e with
    Value(v) ->  if Environment.isIn id state.env then
      let tVar = Utils.getTypeVar id state.env in let tVal = Utils.getTypeVal v in
      if Utils.isSubtype tVal tVar then let nEnv = Environment.update id {typ=tVar;value=v} state.env in {state with env = nEnv;e=Value(VoidV)}
      else rerr ("Invalid types")
    else rerr (id ^ "not declared.")
  | _ -> let ns = (step {state with e = e}) in {ns with e = VariableAssignment(id,ns.e) }
and
  stepSequence (e1:exp) (e2:exp) (state:state) :state = match e1 with
    Value(v) -> { state with e = e2 }
  | _ -> let ns = (step {state with e=e1}) in {ns with e = Sequence(ns.e,e2)}
and
  stepBlockExpression (l: ((typ * id) list)) (exp:exp) (state:state):state = match l with
    [] -> { state with e = exp }
  | [(typ,id)] -> {state with env = (Environment.extend id {typ=typ;value=Utils.initValue typ} state.env);
                              e = Ret(id,exp)}
  | (typ,id)::tl -> {state with env = (Environment.extend id {typ=typ;value=Utils.initValue typ} state.env);
                                 e = Ret(id,BlockExpression(tl,exp))}
and
  stepRet (v:id) (exp:exp) (state:state) :state = if Utils.isValue exp then {state with env = (Environment.pop v state.env);
                                                                                        e = exp}
                                                                       else let ns = (step {state with e = exp }) in {ns with e = Ret(v,ns.e)}

let rec multistep (state:state) : value = match state.e with
  | Value(v) -> v
  | exp -> multistep (step state)

let interpret (e:exp) (program:program) : value =
  let initialEnv = (Environment.extend "a" {typ=IntType;value=IntV 3} Environment.empty) in let initialState = {heap = Heap.empty; env=initialEnv; e=e; prog=program} in multistep initialState

let prg = Program( [Class ("a","b",[],[])] )
let _ = assert (IntV 21 = interpret (Sequence(Value(IntV 22),Value(IntV 21) )) prg )
let _ = assert (IntV 22 = interpret (Sequence(VariableAssignment("a",Value(IntV 22)),Variable("a") )) prg )
let _ = assert (IntV 666 = interpret (Sequence(VariableAssignment("a",Sequence(Value(IntV 22),Value(IntV 666) )),Variable("a") )) prg )
let _ = assert (IntV 1 = interpret (BlockExpression([(IntType,"b")], Variable("b") )) prg )
let _ = assert (IntV 3 = interpret (Variable "a") prg )
