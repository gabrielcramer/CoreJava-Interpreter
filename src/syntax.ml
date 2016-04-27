type program = Program of (classDeclaration list)

and classDeclaration = Class of id * id * ((id * typ) list) * (methodDeclaration list)

and fieldDeclaration = Field of typ * id

and typ = IntType
        | FloatType
        | BoolType
        | VoidType
        | NullType
        | LocType
        | ObjectType of id

and methodDeclaration = Method of typ * id * ((id * typ) list) * exp
                      (* TODO: is MainMethod really necessary? *)
                      | MainMethod of typ * ((id *typ) list) * exp

and exp = Value of value
        | Variable of id
        | ObjectField of id * id
        | VariableAssignment of id * exp
        | ObjectFieldAssignment of (id * id) * exp
        | BlockExpression of ((id * typ) list) * exp
        | Sequence of exp * exp
        | If of id * exp * exp
        | Operation of exp * binaryOperator * exp
        | Negation of exp
        | New of id * (id list)
        | MethodCall of id * id * (id list)
        | While of id * exp
        | Cast of id * id
        | InstanceOf of id * id
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
