type t
type heapEntry

val empty : t
val extend : Syntax.value -> heapEntry -> t -> t
val union : (Syntax.value * heapEntry) list -> t -> t
val isIn : Syntax.value -> t -> bool
val lookup : Syntax.value -> t -> heapEntry
val getFieldEnv : Syntax.value -> t -> Syntax.value Environment.t
