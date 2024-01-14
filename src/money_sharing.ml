
open Graph

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