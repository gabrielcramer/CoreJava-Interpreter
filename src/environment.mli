type 'a t

exception Not_bound

val empty : 'a t
val extend : Syntax.id -> 'a -> 'a t -> 'a t
val union : (Syntax.id * 'a) list -> 'a t -> 'a t
val isIn : Syntax.id -> 'a t -> bool
val lookup : Syntax.id -> 'a t -> 'a
val update : Syntax.id -> 'a -> 'a t -> 'a t
val map : ('a -> 'b) -> 'a t -> 'b t
