open Syntax
open Securitytypechecker
open Exn2
open Utils

(* Tests for Secure types *)
let prg = Program( [Class ("a", "Object", [("f1",IntType)], []);
                    Class ("b", "a", [("f2",IntType)], []);Class ("c", "b", [], []);Class ("d", "Object", [], [])] )
let ste = (Environment.union [("myInstanceAH",{typ = ObjectType "a"; label=H});
                              ("myInstanceAL",{typ = ObjectType "a"; label=L});
                              ("al",{typ = IntType; label=L});
                              ("ah",{typ = IntType; label=H});
                              ("condl",{typ = BoolType; label=L});
                              ("condH",{typ = BoolType; label=H})]
             Environment.empty)
let _ = assert (true = isSubLabel L H)
let _ = assert (false = isSubLabel H L)
let _ = assert (false = isSubLabel (M 3) L)

let _ = assert({typ= VoidType; label = L} = (secureTCExp (VariableAssignment ("al", Value(IntV 3)) ) ste L prg))
let _ = assert({typ= VoidType; label = H} =(secureTCExp (VariableAssignment ("ah", Value(IntV 3)) ) ste L prg))

(* let _ = assert ( let st = Typechecker.secureTypeCheckExp (If("condH",(VariableAssignment ("ah", Variable("al"))),(VariableAssignment ("ah", Value(IntV 3))))) ste L prg in
                 st= {typ=VoidType;label=H })
   let _ = assert ( let st = Typechecker.secureTypeCheckExp (If("condH",(VariableAssignment ("al", Variable("al"))),(VariableAssignment ("al", Variable("al"))))) ste L prg in
                 st= {typ=VoidType;label=H })
   let _ = try let _ = Typechecker.secureTypeCheckExp (VariableAssignment ("al", Value(IntV 3)) ) ste H prg in () with StaticError msg -> print_endline msg
   let _ = try let _ = Typechecker.secureTypeCheckExp (If("condH",(VariableAssignment ("al", Variable("ah"))),(VariableAssignment ("al", Variable("al"))))) ste L prg in () with StaticError msg -> print_endline msg
   let _ = try let _ = Typechecker.secureTypeCheckExp (If("condH",(VariableAssignment ("al", Value(IntV 3))),(VariableAssignment ("al", Variable("al"))))) ste L prg in () with StaticError msg -> print_endline msg *)

let _ = assert (H=(Utils.lubLabel H L))
let _ = assert (H=(Utils.lubLabel L H))
let _ = assert (L= (Utils.lubLabel L L))

let _ = assert (L= (Utils.glbLabel H L))
let _ = assert (L= (Utils.glbLabel L H))
let _ = assert (L= (Utils.glbLabel L L))

(* let _ = print_endline (Utils.stringOfSecureType (Typechecker.secureTypeCheckExp (ObjectField("myInstanceAL","f1")) ste L prg))
let _ = print_endline (Utils.stringOfSecureType (Typechecker.secureTypeCheckExp (ObjectField("myInstanceAH","f1")) ste L prg)) *)
(* print_endline (Utils.stringOfSecureType st); *)
