type program = Program of (classDeclaration list)

and classDeclaration = Class of id * id * ((typ * id) list) * (methodDeclaration list)

and fieldDeclaration = Field of typ * id

and typ = IntType
        | FloatType
        | BoolType
        | VoidType
        | NullType
        | LocType
        | ObjectType of id

and methodDeclaration = Method of typ * id * ((typ * id) list) * exp
                      | MainMethod of typ * ((typ * id) list) * exp

and exp = Value of value
        | Variable of id
        | ObjectField of id * id
        | VariableAssignment of id * exp
        | ObjectFieldAssignment of (id * id) * exp
        (* | LocalVariableDeclaration of typ * id * exp *)
        | BlockExpression of ((typ * id) list) * exp
        | Sequence of exp * exp
        | If of id * exp * exp
        | Operation of exp * binaryOperator * exp
        | Negation of exp
        | New of id * (id list)
        | MethodCall of id * id * (id list)
        | While of id * exp
        | Cast of typ * id
        | InstanceOf of id * typ
        | Ret of id * exp
(* and variableDeclaration = VariableDeclaration of typ * id *)
(* and blkExp = BlockExpression of *)
(* | BnVar of exp *)

and value = NullV
          | IntV of int
          | FloatV of float
          | BoolV of bool
          | VoidV
          | LocV of int

and typeValue =
  {	typ: typ;
    value: value
  }
and binaryOperator = IPlus
                   | IMinus
                   | IMultiply
                   | IDivide
                   | FPlus
                   | FMinus
                   | FMultiply
                   | FDivide
                   | Less
                   | LessEqual
                   | EqEqual
                   | GreaterEqual
                   | Greater
                   | NotEqual
                   | And
                   | Or

and id = string;;
