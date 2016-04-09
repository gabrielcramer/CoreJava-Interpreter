type t = (Syntax.value * heapEntry) list
(* Problem: the key should be just LocV *)
and heapEntry = {id: Syntax.id; env: Syntax.typeValue Environment.t}
exception Not_bound

let empty = []
let extend x v heap = (x,v)::heap

let rec union lst heap =
  match lst with
    [] -> heap
  | (x::xs) -> x :: union xs heap

let isIn loc heap =
  try List.assoc loc heap;true with Not_found -> false

let lookup id heap =
  try List.assoc id heap with Not_found -> raise Not_bound

let getFieldEnv loc heap =
  try let objVal = List.assoc loc heap in objVal.env with Not_found -> raise Not_bound
