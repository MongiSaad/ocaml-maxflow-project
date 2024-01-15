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
  let graph4 = rm_nullarc graph in (*au cas ou il y a déja un arc nul au début de l'algo*)
  let path = find_path graph4 source puit [] in
  (*print_list_of_int path;*)
  match path with 
  | [] -> graph
  | nodelist -> let arclbllist = node_to_arclbl_list nodelist graph [] in
    (*print_list_of_int arclbllist;*)
    let arclist = node_to_arc_list nodelist graph [] in
    let v = augmentation arclbllist in
    (*Printf.printf "%d\n%!" v;*)
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




  let flowcapa_graph graph =
    gmap graph (fun x -> (0., x))
  
  let test_full_lbl (flow,capa) =
    flow<>capa

  let rm_fullarcfloat gr = gfilter gr (fun arc -> test_full_lbl arc.lbl)

  let rec augmentationfloat l =
    let minvalue = Float.max_float in
    match l with
    | [] -> minvalue
    | (_,capa)::[] -> capa
    | (_,capa)::rest -> let minvalue = augmentationfloat rest in
                        if capa > minvalue then minvalue
                        else capa

let new_lbl (flow,capa) v = (flow+.v,capa)

let rec augmenterfloat v arclist graph =
  match arclist with
  | [] -> graph
  | x::rest ->
    let graph2 = new_arc graph {src=x.src; tgt=x.tgt;lbl=new_lbl x.lbl v} in
      augmenterfloat v rest graph2

let print_list_of_tuplef path = match path with
  | [] -> Printf.printf "[]"
  | _ -> let l1 = List.map (fun (flow,capa) -> "("^string_of_float(flow)^","^string_of_float(capa)^")") path in
    Printf.printf "%s\n%!" (String.concat " " l1)
  
  let rec ffalgofloat graph source puit =
    let graph4 = rm_fullarcfloat graph in (*au cas ou il y a déja un arc nul au début de l'algo*)
    let path = find_path graph4 source puit [] in
    print_list_of_int path;
    match path with 
    | [] -> graph
    | nodelist -> let arclbllist = node_to_arclbl_list nodelist graph [] in
    print_list_of_tuplef arclbllist;
      let arclist = node_to_arc_list nodelist graph [] in
      let v = augmentationfloat arclbllist in
      Printf.printf "%f\n%!" v;
      let graph2 = augmenterfloat v arclist graph in
      let graph3 = rm_fullarcfloat graph2 in
      ffalgofloat graph3 source puit

let getcapa (_,capa) = capa
let getflow (flow,_) = flow

let gsolfloat gr1 gr2 = let gr3 = clone_nodes gr1 in
e_fold gr1 (fun gr4 arc -> (new_arc gr4 {src=arc.src; tgt=arc.tgt; lbl= match find_arc gr2 arc.src arc.tgt with
  | None -> string_of_float(getcapa(arc.lbl))^"/"^string_of_float(getcapa(arc.lbl))
  | Some arc2 ->if (getflow(arc.lbl)-.getflow(arc2.lbl)>=0.)
      then string_of_float(getflow(arc.lbl)-.getflow(arc2.lbl))^"/"^string_of_float(getcapa(arc.lbl))
      else string_of_float(getflow(arc2.lbl))^"/"^string_of_float(getcapa(arc.lbl))})) gr3

let solutionfloat graph source puit =
  let graph3 = ffalgofloat graph source puit in
  gsolfloat graph graph3