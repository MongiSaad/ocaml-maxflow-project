open Graph

val txt_to_node: string -> 'a graph

val txt_to_amount: string -> float * string list * float list

val due_per_person: 'a graph -> float -> float

val diff_person: float list -> float -> float list

val create_graph_complet: string graph -> id list -> string graph

val node_list_from_graph: 'a graph -> id list

val create_source_puit: float graph -> id list -> float list -> float graph

val algo_money_sharing: string -> string -> unit