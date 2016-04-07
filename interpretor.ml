open AbstractSyntaxTree

(* type state = heap * stack * exp *)
type heap = (int * objValue) list
and objValue = {id:id; stack:stack}
and stack = (id * typeValue) list
exception TypeError of string
exception RuntimeError of string
(* (varname * value)list *)

(* provisory implementation *)
let rec lookup (id:id) (env:stack) : value = match env with
    [] -> raise (RuntimeError("Id not declared."))
  | (hd1, hd2)::tl -> if hd1 = id then let (_,value) = hd2 in value else lookup id tl

let rec isIn (id:id) (env:stack): bool = match env with
    [] -> false
  | (hd1, _)::tl -> if hd1 = id then true else isIn id tl


let rec isInHeap (loc:int) (h:heap) : bool = match h with
    [] -> false
  | (hd1, _)::tl -> if hd1 = loc then true else isInHeap loc tl


let rec getFieldEnv (location:value) (h:heap) : stack = match h,location with
    [],_ ->raise (RuntimeError ("Location not in the heap"))
  | (hd1, hd2)::tl, LocV(loc) -> if hd1 = loc then hd2.stack else getFieldEnv location tl

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

let applyOp (bop: binaryOperator) (v1:value) (v2:value) :value = match bop with
    IPlus -> (match v1,v2 with
        IntV int1,IntV int2 -> IntV(int1 + int2))
  | IMinus -> ( match v1,v2 with
        IntV(int1),IntV(int2) -> IntV(int1 - int2))
  | IMultiply -> (match v1, v2 with
        IntV(int1), IntV(int2) -> IntV(int1 * int2))
  | IDivide -> (match v1, v2 with
        IntV(int1), IntV(int2) -> IntV(int1 / int2))
  (* TODO all the bops *)
  |_ -> NullV;;


let rec eval (e:exp) (h:heap) (env:stack) (prog:program) : value = match e with
    Value(v) -> v;
  | Variable(id) -> if isIn id env then lookup id env
    else raise(RuntimeError (id ^ "not declared."))
  | ObjectField(var,field) -> let loc = lookup var env in if isLocation loc then let fldE = getFieldEnv loc h in let v = lookup field fldE in v
    else raise(RuntimeError "Not a location");
  | Operation(exp1, op, exp2) -> applyOp op (eval exp1 h env prog) (eval exp2 h env prog);
