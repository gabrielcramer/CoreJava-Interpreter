open Syntax
open Utils
open Exn2

let rec secureTCExp exp stenv hl prog = match exp with
  | Value(v) -> {typ = Utils.getTypeOfVal v; label=L}
  | Variable(var) -> secureTCVariable var stenv
  (* | ObjectField(var, field) -> secureTypeCheckObjectFieldExp var field stenv hl prog (*TODO*) *)
  | VariableAssignment (var, exp)-> secureTCVariableAssignment var exp stenv hl prog
  | ObjectFieldAssignment((var, f), e) -> {typ = IntType; label=L} (*TODO*)
  | Sequence(e1, e2) -> {typ = IntType; label=L} (*TODO*)
  | BlockExpression(list, exp) -> {typ = IntType; label=L} (*TODO*)
  (* | If (var, et, ee) -> secureTypeCheckIfExp var et ee stenv L prog *)
  | Operation(e1, op, e2) -> {typ = IntType; label=L} (*TODO*)
  | Negation(e) -> {typ = IntType; label=L} (*TODO*)
  | New(cn, varList) -> {typ = IntType; label=L} (*TODO*)
  | While(var, e) -> {typ = IntType; label=L} (*TODO*)
  | Cast(cn, var) -> {typ = IntType; label=L} (*TODO*)
  | InstanceOf(var, cn) -> {typ = IntType; label=L} (*TODO*)
  | MethodCall(cn, mn, params) -> {typ = IntType; label=L} (*TODO*)
  | Ret(v, exp) -> raiseStaticError ("Expression 'Ret' should not occur while type checking.")


and secureTCVariable var stenv = if Environment.isIn var stenv then
    Environment.lookup var stenv
  else raiseUnboundVar var

and secureTCVariableAssignment var exp stenv hl prog = if var <> "this" then let varType = secureTCVariable var stenv in
    let expType = secureTCExp exp stenv hl prog in if isSecureSubtype expType varType prog then {varType with typ = VoidType}
    else raiseStaticError ("Insecure information flow from " ^ (Utils.stringOfExp exp) ^ " to " ^ var)
  else raiseStaticError ("Cannot assign a value to final variable this.")
