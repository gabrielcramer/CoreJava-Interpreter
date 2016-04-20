exception RuntimeError of string
val getTypeVar: Syntax.id -> Syntax.typeValue Environment.t -> Syntax.typ
val getTypeVal: Syntax.value -> Syntax.typ
val getTypeField: Syntax.typ -> Syntax.id -> Syntax.program -> Syntax.typ option
val isSubtype: Syntax.typ -> Syntax.typ -> Syntax.program -> bool

val leastMaxType: Syntax.typ -> Syntax.typ -> Syntax.program -> Syntax.typ option

val initValue: Syntax.typ -> Syntax.value
val isValue: Syntax.exp -> bool
val isIntOperator: Syntax.binaryOperator -> bool
val isFloatOperator: Syntax.binaryOperator -> bool
val isCompOperator: Syntax.binaryOperator -> bool
val isBoolOperator: Syntax.binaryOperator -> bool
val isObjectType: Syntax.typ -> bool
val definedInProg: Syntax.id -> Syntax.program -> bool
val firstUnboundVariable: Syntax.id list -> Syntax.typeValue Environment.t -> Syntax.id option
val getFieldList: Syntax.typ -> Syntax.program-> (Syntax.id * Syntax.typ) list
val getTypeList: Syntax.id list -> Syntax.typeValue Environment.t -> Syntax.typ list
val definedInProgAux: Syntax.id -> Syntax.classDeclaration list -> bool
val checkFieldsTypes: (Syntax.id * Syntax.typ) list -> Syntax.typ list -> Syntax.program -> Syntax.id option
val createFieldEnv: (Syntax.id * Syntax.typ) list -> Syntax.id list -> Syntax.typeValue Environment.t -> Syntax.typeValue Environment.t
val getParent: Syntax.typ -> Syntax.program -> Syntax.typ


val stringOfType: Syntax.typ -> string
val stringListOfIdTypList: (Syntax.id * Syntax.typ) list -> string list
val stringOfValue: Syntax.value -> string
val stringOfEnv: Syntax.typeValue Environment.t -> string
val stringOfExp: Syntax.exp -> string
val stringOfMethods: Syntax.methodDeclaration list -> string

val rerr: string -> 'a
val raiseDifferentTypeExpErr: Syntax.exp -> Syntax.typ -> Syntax.typ -> 'a
