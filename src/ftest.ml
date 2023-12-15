open Gfile
(*open Tools*)
open Ford_fulkerson
    
let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf
        "\n ✻  Usage: %s infile source sink outfile\n\n%s%!" Sys.argv.(0)
        ("    🟄  infile  : input file containing a graph\n" ^
         "    🟄  source  : identifier of the source vertex (used by the ford-fulkerson algorithm)\n" ^
         "    🟄  sink    : identifier of the sink vertex (ditto)\n" ^
         "    🟄  outfile : output file in which the result should be written.\n\n") ;
      exit 0
    end ;


  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)
  
  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)
  
  (* These command-line arguments are not used for the moment. *)
  and _source = int_of_string Sys.argv.(2)
  and _sink = int_of_string Sys.argv.(3)
  in

  (* Open file *)
  let graph = from_file infile in

  (*fonctions à tester*)
 (* let graph2 = gmap graph (fun x -> string_of_int(int_of_string(x)+1)) in*)

 (*let graph2 = gmap graph (fun x -> int_of_string(x)) in
 let graph3 = add_arc graph2 4 5 6 in
 let graph4 = gmap graph3 (fun x -> string_of_int(x)) in*)

 (*let () = export infile outfile in
 ()*)
  let chemin = find_path graph 0 5 [] in
  let l = List.map (fun x -> string_of_int(x)) chemin in
  Printf.printf "%s\n%!" (String.concat " " l);

  (* Rewrite the graph that has been read. *)
  let () = write_file outfile graph in

  ()

