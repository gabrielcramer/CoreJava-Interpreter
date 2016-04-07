type program = Program of (classDeclaration list)

and classDeclaration = Class of id * id * (fieldDeclaration list) * (methodDeclaration list)

and fieldDeclaration = Field of typ * id

and typ = IntType
	| FloatType
	| BoolType
	| VoidType
	| ObjectType of id

and methodDeclaration = Method of typ * id * ((typ * id) list) * exp
	| MainMethod of typ * ((typ * id) list) * exp

and exp = Value of value
  | Variable of id
  | ObjectField of id * id
  | VariableAssignment of id * exp
  | ObjectFieldAssignment of (id * id) * exp
  | LocalVariableDeclaration of typ * id * exp
	| Blk of blkExp
  | Sequence of exp * exp
  | If of id * exp * exp
	| Operation of exp * binaryOperator * exp
  | Negation of exp
  | New of id * (id list)
  | MethodCall of id * id * (id list)
  | While of id * exp
  | Cast of typ * id
  | InstanceOf of id * typ

and blkExp = Bvar of typ * id * exp
 	| BnVar of exp

and value = NullV
	| IntV of int
	| FloatV of float
	| BoolV of bool
	| VoidV
	| LocV of int

and typeValue = typ * value

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
