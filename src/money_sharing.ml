open Graph
open Tools
open Ford_fulkerson
open Gfile

let read_name graph line id=
  try Scanf.sscanf line "%s %f" (fun _ _ -> new_node graph id)
  with e ->
    Printf.printf "Cannot read node in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file"

let txt_to_node infile =

  let infile = open_in infile in

  (* Read all lines until end of file. *)
  let rec loop graph nb_node=
    try
      let line = input_line infile in

      (* Remove leading and trailing spaces. *)
      let line = String.trim line in

      let graph2 =
        (* Ignore empty lines *)
        if line = "" then graph
        
        else read_name graph line nb_node
      in      
      loop graph2 (nb_node+1)

    with End_of_file -> graph (* Done *)
  in

  let final_graph = loop empty_graph 0 in

  close_in infile ;
  final_graph

let calculate_amount line amount accunom accupaye=
  try Scanf.sscanf line "%s %f" (fun n a -> (a+.amount, n::accunom, a::accupaye))
  with e ->
    Printf.printf "Cannot read node in line - %s:\n%s\n%!" (Printexc.to_string e) line ;
    failwith "from_file"

let txt_to_amount infile =

  let infile = open_in infile in

  (* Read all lines until end of file. *)
  let rec loop (amount, accunom, accupaye) = 
    try
      let line = input_line infile in

      (* Remove leading and trailing spaces. *)
      let line = String.trim line in

      let (amount2, accunom2, accupaye2) =
        (* Ignore empty lines *)
        if line = "" then (amount, accunom, accupaye)

        else calculate_amount line amount accunom accupaye
      in      
      loop (amount2, accunom2, accupaye2)

    with End_of_file -> (amount, List.rev(accunom), List.rev(accupaye)) (* Done *)
  in

  let final_amount = loop (0., [], []) in

  close_in infile ;
  final_amount

  let due_per_person graph amount =
    let nb_node = n_fold graph (fun nb n -> max nb n) 0 in
    amount/.(float_of_int(nb_node)+.1.)

let diff_person ldue v =
  List.map (fun paid -> paid-.v) ldue

  let node_list_from_graph graph =
    n_fold graph (fun accu n -> n::accu) []

let liste_arc_inf nodes_list =
  let add_pairs acc x =
    acc @ List.map (fun y -> (x, y)) (List.filter (fun z -> z<>x) nodes_list)
  in
  List.fold_left add_pairs [] nodes_list

let create_graph_complet graph nodes_list =
  let pairs = liste_arc_inf nodes_list in
  let rec create_arcs graph  = function
    | [] -> graph
    | (x,y)::rest -> create_arcs (new_arc graph {src=x; tgt=y; lbl=string_of_float(Float.infinity)}) rest 
  in create_arcs graph pairs

let rec max_list l =
  let minvalue = Int.min_int in
  match l with
  | [] -> minvalue
  | x::[] -> x
  | x::rest -> let minvalue = max_list rest in
                      if x < minvalue then minvalue
                      else x

let create_source_puit graph nodelist lpaid=
  let max_list = max_list nodelist in
  Printf.printf "%d\n%!" max_list;
  let gr2 = new_node graph (max_list+1) in
  let gr3 = new_node gr2 (max_list+2) in
  let rec link nb_node graph= function
    | [] -> graph
    | x::rest -> let gr4 = if x<=0. then new_arc graph {src=(max_list+1); tgt=nb_node; lbl=Float.abs(x)} 
      else new_arc graph {src=nb_node; tgt=(max_list+2); lbl=x} in
      link (nb_node+1) gr4 rest in
      link 0 gr3 lpaid

let algo_money_sharing infile outfile = 
  let ms_graph = txt_to_node infile in
  let (a, lnom, lpaid) = txt_to_amount infile in
  let v = due_per_person ms_graph a in
  let ldiff = diff_person lpaid v in
  let listnodes = node_list_from_graph ms_graph in
  let graph2 = create_graph_complet ms_graph listnodes in
  let graph3 = gmap graph2 (fun x -> float_of_string(x)) in
  let graph4 = create_source_puit graph3 listnodes ldiff in
  let graph5 = flowcapa_graph graph4 in
  let nb_node = max_list listnodes in
  let fgraph = solutionfloat graph5 (nb_node+1) (nb_node+2) in
  export2 fgraph (lnom @ ["source"; "puit"]) outfile