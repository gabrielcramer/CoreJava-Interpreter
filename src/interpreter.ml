open Syntax
(* type state = heap * stack * exp *)

type state =
  { e: exp;
    heap: Heap.t;
    env: Syntax.value Environment.t;
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

let rec getTypeVal (v:value) = match v with
  (* TODO: Think about Null and Void *)
    IntV _s -> IntType
  | FloatV _ -> FloatType
  | BoolV _ -> BoolType
  | VoidV -> VoidType
  | _ -> failwith "Think about Null and Void "

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
    Variable(id) -> if Environment.isIn id state.env then let v = Environment.lookup id state.env in {state with e = Value(v)}
    else raise (RuntimeError (id ^ "not declared."))


  | ObjectField(var,field) -> if Environment.isIn var state.env
    then let loc = Environment.lookup var state.env  in
      (* TODO: check if location is in heap *)
      if isLocation loc then let fldE = (Heap.getFieldEnv loc state.heap) in
        if Environment.isIn field fldE then let v = Environment.lookup field fldE in {state with e = Value(v)}
        else raise (RuntimeError("Field:" ^ field ^ "not declared inside " ^ var))
      else rerr (var ^ "is not an object.")
    else raise (RuntimeError (var ^ "not declared."))


(* | VariableAssignment(id,exp) -> stepVariableAssignment id exp h env prog  *)
(*| Operation(exp1, op, exp2) -> applyOp op (eval exp1 h env prog) (eval exp2 h env prog);; *)

(* and

   stepVariableAssignment (id:id) (e:exp) (h:heap) (env:stack) (prog:program) = match e with
    | Value(v) ->  if id isIn env then if getType id env

                                  else *)
let rec multistep (state:state) : value = match state.e with
  | Value(v) -> v
  | exp -> multistep (step state)

let interpret (e:exp) (program:program) : value =
  let initialEnv = (Environment.extend "a" (IntV 3) Environment.empty) in let initialState = {heap = Heap.empty; env=initialEnv; e=e; prog=program} in multistep initialState

let prg = Program( [Class ("a","b",[],[])] )
let _ = assert (IntV 22 = interpret (Value(IntV 22)) prg )
let _ = assert (IntV 3 = interpret (Variable "a") prg )
