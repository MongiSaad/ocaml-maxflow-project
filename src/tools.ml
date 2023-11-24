(* Yes, we have to repeat open Graph. *)
open Graph

(* assert false is of type ∀α.α, so the type-checker is happy. *)
let clone_nodes (gr:'a graph) = n_fold gr new_node empty_graph

let gmap gr f = let gr2 = clone_nodes gr in
  e_fold gr (fun gr3 arc -> (new_arc gr3 {src=arc.src; tgt=arc.tgt; lbl=f arc.lbl})) gr2

let add_arc g id1 id2 n = if (find_arc g id1 id2 = None)
    then new_arc gr2 {src=id1; tgt=id2; lbl=n} else gmap gr (f x -> x+n)