open Syntax
open Securitytypechecker
open Exn2
open Utils

(* Tests for Secure types *)
let classA = Class ("a",L,"Object", [("f1",{typ= IntType;label=L})],
                    [Method({typ= IntType;label=L},
                            "add",[("p1",{typ= IntType;label=L})],
                            L,Value(IntV 3))])
let prg = Program [classA;
                   Class ("b",H, "a", [("f2",{typ= IntType;label=L})], []);
                   Class ("c",L ,"b", [], []);
                   Class ("d",L, "Object", [], [])]
let ste = (Environment.union [("myInstanceAH",{typ = ObjectType "a"; label=H});
                              ("myInstanceAL",{typ = ObjectType "a"; label=L});
                              ("al",{typ = IntType; label=L});
                              ("ah",{typ = IntType; label=H});
                              ("condL",{typ = BoolType; label=L});
                              ("condH",{typ = BoolType; label=H})]
             Environment.empty)
let _ = assert (true = isSubLabel L H)
let _ = assert (false = isSubLabel H L)
let _ = assert (false = isSubLabel (M 3) L)

let _ = assert ({typ= VoidType; label = L} = (secureTCExp (VariableAssignment ("al", Value(IntV 3)) ) ste L prg))
let _ = assert ({typ= VoidType; label = H} =(secureTCExp (VariableAssignment ("ah", Value(IntV 3)) ) ste L prg))
let _ = assert ({typ= IntType; label = L} = secureTCExp (ObjectField ("myInstanceAL","f1")) ste L prg)
let _ = assert ({typ= IntType; label = H} = secureTCExp (ObjectField ("myInstanceAH","f1")) ste L prg)

let _ = assert ({typ= VoidType; label = L} = (secureTCExp (ObjectFieldAssignment (("myInstanceAL","f1"), Value(IntV 3)) ) ste L prg))
(* let _ = assert ({typ= VoidType; label = L} = (secureTCExp (ObjectFieldAssignment (("myInstanceAL","f1"), (ObjectField ("myInstanceAH","f1")) ) ) ste H prg)) *)

let _ = assert ( let st = secureTCExp (If("condH",(VariableAssignment ("ah", Variable("al"))),(VariableAssignment ("ah", Value(IntV 3))))) ste H prg in
                 st= {typ=VoidType;label=H })
let _ = assert ( let st = secureTCExp (If("condL",(VariableAssignment ("al", Variable("al"))),(VariableAssignment ("al", Variable("al"))))) ste H prg in
                 st= {typ=VoidType;label=L })
let _ = try let _ = secureTCExp (VariableAssignment ("al", Value(IntV 3)) ) ste H prg in () with StaticError msg -> print_endline msg
let _ = try let _ = secureTCExp (If("condH",(VariableAssignment ("al", Variable("ah"))),(VariableAssignment ("al", Variable("al"))))) ste L prg in () with StaticError msg -> print_endline msg
let _ = try let _ = secureTCExp (If("condL",(VariableAssignment ("al", Value(IntV 3))),(VariableAssignment ("al", Variable("al"))))) ste H prg in () with StaticError msg -> print_endline msg


let _ = assert ({typ= VoidType; label = L} = (secureTCExp (Sequence (
    (VariableAssignment ("al", Value(IntV 3)) ),
    (VariableAssignment ("ah", Value(IntV 3)) )
  )) ste L prg))

let _ = assert ({typ= BoolType; label= L} = secureTCExp (BlockExpression ([("newBool",{typ= BoolType; label= L})], Variable "newBool"))
                  ste L prg)

let _ = try
    let _ = assert ({typ= IntType; label= L} = secureTCExp (MethodCall ("myInstanceAL", "add", ["al"]))
                      ste L prg) in
    let _ = assert ({typ= IntType; label= H} = secureTCExp (MethodCall ("myInstanceAH", "add", ["al"]))
                      ste L prg) in

    ()
  with
    StaticError msg -> print_endline ("MethodCall: " ^ msg)


let _ = assert ({typ=ObjectType "a"; label = L} = secureTCExp(New("a",["al"]))
                  ste L prg)
let _ = assert ({typ=ObjectType "b"; label = H} = secureTCExp(New("b",["al";"al"]))
                  ste H prg)

let _ = assert ({typ= BoolType; label=L} = secureTCExp (Operation ((Value(IntV 3)),EqEqual,(Value(IntV 3)))) ste L prg)

let _ = assert ({typ= BoolType; label=L} = secureTCExp (Operation ((Value(IntV 3)),EqEqual,(Value(IntV 3)))) ste H prg)

let _ = assert (H=(Utils.lubLabel H L))
let _ = assert (H=(Utils.lubLabel L H))
let _ = assert (L= (Utils.lubLabel L L))

let _ = assert (L= (Utils.glbLabel H L))
let _ = assert (L= (Utils.glbLabel L H))
let _ = assert (L= (Utils.glbLabel L L))
let _ = try wellTypedClass classA prg; print_endline "Class A is wellTypedMethod" with
  | BadTypedMethod Method(mt,mn, _, _, _) -> raise (StaticError ("Method " ^ mn ^ " is not well typed."))
(* let _ = print_endline (Utils.stringOfSecureType (secureTCExp (ObjectField("myInstanceAL","f1")) ste L prg))
   let _ = print_endline (Utils.stringOfSecureType (secureTCExp (ObjectField("myInstanceAH","f1")) ste L prg)) *)
(* print_endline (Utils.stringOfSecureType st); *)
