open Gfile
open Graph

val txt_to_node: path -> path graph

val txt_to_amount: string -> float * string list * float list

val due_per_person: 'a graph -> float -> float

val diff_person: float list -> float -> float list