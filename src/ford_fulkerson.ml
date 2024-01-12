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

let rm_nullarc gr = gfilter gr (fun arc -> arc.lbl>0)

let print_list_of_int path = match path with
  | [] -> Printf.printf "[]"
  | _ -> let l1 = List.map (fun x -> string_of_int(x)) path in
    Printf.printf "%s\n%!" (String.concat " " l1)


let rec ffalgo graph source puit =
  let path = find_path graph source puit [] in
  print_list_of_int path;
  match path with 
  | [] -> graph
  | nodelist -> let arclbllist = node_to_arclbl_list nodelist graph [] in
    print_list_of_int arclbllist;
    let arclist = node_to_arc_list nodelist graph [] in
    let v = augmentation arclbllist in
    Printf.printf "%d\n%!" v;
    let graph2 = augmenter v arclist graph in
    let graph3 = rm_nullarc graph2 in
    ffalgo graph3 source puit

let gsol gr1 gr2 = let gr3 = clone_nodes gr1 in
  e_fold gr1 (fun gr4 arc -> (new_arc gr4 {src=arc.src; tgt=arc.tgt; lbl= match find_arc gr2 arc.src arc.tgt with
    | None -> string_of_int(arc.lbl)^"/"^string_of_int(arc.lbl)
    | Some arc2 -> if (arc.lbl-arc2.lbl>=0)
        then string_of_int(arc.lbl-arc2.lbl)^"/"^string_of_int(arc.lbl)
        else "0/"^string_of_int(arc.lbl)})) gr3

let solution graph source puit =
  let graph2 = ffalgo graph source puit in
  gsol graph graph2