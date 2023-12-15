(* Yes, we have to repeat open Graph. *)
open Graph

let rec find_path graph s p accu =
  if s != p then
    let arcs_sortant = out_arcs graph s in
    iter_arcs arcs_sortant graph p accu
  else accu
and iter_arcs l graph p accu = match l with
    | [] -> []
    | arc::rest -> let res = find_path graph (arc.tgt) p (arc.src::accu) in
      match res with
      | [] -> iter_arcs rest graph p accu
      | _ -> res

      ajouter d à la fin + gerer les cycles + reverse la liste obtenue