open Graph

val find_path: 'a graph -> id -> id -> id list -> id list

val node_to_arclbl_list: id list -> 'a graph -> 'a list -> 'a list

val node_to_arc_list: id list -> 'a graph -> 'a arc list -> 'a arc list

val augmentation: id list -> id

val augmenter: id -> id arc list -> id graph -> id graph

val rm_nullarc: id graph -> bool graph