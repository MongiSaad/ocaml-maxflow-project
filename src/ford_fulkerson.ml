(* Yes, we have to repeat open Graph. *)
open Graph
open Tools

let rec find_path graph s p accu =
  if s != p then
    let arcs_sortant = out_arcs graph s in
    iter_arcs arcs_sortant graph p accu
  else List.rev (p::accu)
and iter_arcs l graph p accu = match l with
  | [] -> []
  | arc::rest -> let res = if List.exists (fun x -> x = arc.tgt) accu then (*pour éviter les cycles à tester !!! *)
                     [] else find_path graph (arc.tgt) p (arc.src::accu) in
    match res with
    | [] -> iter_arcs rest graph p accu
    | _ -> res

let rec node_to_arclbl_list l graph accu = match l with
  | [] -> accu
  | x::rest -> match rest with
    | [] -> accu
    | y::rest2 -> let res = (find_arc graph x y) in match res with
      | None -> failwith "Arc inexistant !\n"
      | Some x -> node_to_arclbl_list (y::rest2) graph (x.lbl::accu)

let rec node_to_arc_list l graph accu = match l with
| [] -> accu
| x::rest -> match rest with
  | [] -> accu
  | y::rest2 -> let res = (find_arc graph x y) in match res with
    | None -> failwith "Arc inexistant !\n"
    | Some x -> node_to_arc_list (y::rest2) graph (x::accu)


let rec augmentation l =
  let minvalue = Int.max_int in
  match l with
  | [] -> minvalue
  | x::[] -> x
  | x::rest -> let minvalue = augmentation rest in
                      if x > minvalue then minvalue
                      else x

let rec augmenter v arclist graph =
  match arclist with
  | [] -> graph
  | x::rest -> let graph2 = add_arc graph x.src x.tgt (-v) in
      let graph3 = add_arc graph2 x.tgt x.src v in
      augmenter v rest graph3

let rm_nullarc gr = let gr2 = clone_nodes gr in
e_fold gr (fun gr3 arc -> if (arc.lbl>0) then (new_arc gr3 {src=arc.src; tgt=arc.tgt; lbl=arc.lbl}) else ) gr2 