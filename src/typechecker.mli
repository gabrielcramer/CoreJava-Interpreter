val typeCheckProgram: Syntax.program -> unit
val typeCheckExp: Syntax.exp -> Syntax.typ Environment.t-> Syntax.program -> Syntax.typ
val secureTypeCheckExp: Syntax.exp -> Syntax.secureType Environment.t -> Syntax.label -> Syntax.program -> Syntax.secureType
val typeCheckObjectFieldExp: Syntax.id -> Syntax.id -> Syntax.typ Environment.t -> Syntax.program -> Syntax.typ
val typeCheckVariableAssignmentExp: Syntax.id -> Syntax.exp -> Syntax.typ Environment.t -> Syntax.program -> Syntax.typ
