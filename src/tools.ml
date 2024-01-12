(* Yes, we have to repeat open Graph. *)
open Graph

(* assert false is of type ∀α.α, so the type-checker is happy. *)
let clone_nodes (gr:'a graph) = n_fold gr new_node empty_graph

let gmap gr f = let gr2 = clone_nodes gr in
  e_fold gr (fun gr3 arc -> (new_arc gr3 {src=arc.src; tgt=arc.tgt; lbl=f arc.lbl})) gr2

let add_arc g id1 id2 n = match (find_arc g id1 id2) with
  | None -> new_arc g {src=id1; tgt=id2; lbl=n}
  | Some e -> new_arc g {src=id1; tgt=id2; lbl=n + e.lbl}

let gfilter gr f = let gr2 = clone_nodes gr in
  e_fold gr (fun gr3 arc -> if (f arc) then (new_arc gr3 {src=arc.src; tgt=arc.tgt; lbl=arc.lbl}) else gr3) gr2