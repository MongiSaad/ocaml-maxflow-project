open Graph

val clone_nodes: 'a graph -> 'b graph
val gmap: 'a graph -> ('a -> 'b) -> 'b graph
val add_arc: int graph -> id -> id -> int -> int graph
val gfilter: 'a graph -> ('a arc -> bool) -> 'a graph
val add_arc_float: float graph -> id -> id -> float -> float graph