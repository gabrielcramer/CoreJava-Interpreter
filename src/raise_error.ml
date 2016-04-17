exception RuntimeError of string
let unboundVar var =  raise(RuntimeError ("Error 3: Unbound variable "^ var))
