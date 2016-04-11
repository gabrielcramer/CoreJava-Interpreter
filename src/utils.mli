


val getTypeVar: Syntax.id -> Syntax.typeValue Environment.t -> Syntax.typ
val getTypeVal: Syntax.value -> Syntax.typ
val isSubtype: Syntax.typ -> Syntax.typ -> bool
val initValue: Syntax.typ -> Syntax.value
val isValue: Syntax.exp -> bool
val definedInProg: Syntax.id -> Syntax.program -> bool
val firstUnboundVariable: Syntax.id list -> Syntax.typeValue Environment.t -> Syntax.id option
val getFieldList: Syntax.id -> Syntax.program-> (Syntax.typ* Syntax.id) list
val getTypeList: Syntax.id list -> Syntax.typeValue Environment.t -> Syntax.typ list
val definedInProgAux: Syntax.id -> Syntax.classDeclaration list -> bool
val checkFieldsTypes: (Syntax.typ * Syntax.id) list -> Syntax.typ list -> Syntax.id option
val createFieldEnv: (Syntax.typ* Syntax.id) list -> Syntax.id list -> Syntax.typeValue Environment.t -> Syntax.typeValue Environment.t
val getParent: Syntax.id -> Syntax.program -> Syntax.id

val stringOfType: Syntax.typ -> string
val stringOfValue: Syntax.value -> string
val stringOfEnv: Syntax.typeValue Environment.t -> string
