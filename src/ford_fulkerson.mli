open Graph

val find_path: 'a graph -> id -> id -> id list -> id list

val node_to_arclbl_list: id list -> 'a graph -> 'a list -> 'a list

val node_to_arc_list: id list -> 'a graph -> 'a arc list -> 'a arc list

val augmentation: int list -> int

val augmenter: id -> id arc list -> id graph -> id graph

val rm_nullarc: id graph -> id graph

val ffalgo: id graph -> id -> id -> id graph

val solution: id graph -> id -> id -> string graph

val solutionfloat: (float * float) graph -> id -> id -> string graph

val ffalgofloat:(float * float) graph -> id -> id -> (float * float) graph

val flowcapa_graph: 'a graph -> (float * 'a) graph