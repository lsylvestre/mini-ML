(**************************************************************************)
(*                                                                        *)
(*         PSTL : OCaml sur la plate-forme Nand2Tetris (2020)             *)
(*                                                                        *)      
(*           Loïc SYLVESTRE              Pablito BELLO                    *)
(*           loic.sylvestre@etu.upmc.fr  pablito.bello@etu.upmc.fr        *)
(*                                                                        *)  
(**************************************************************************)

let parse filename = 
  let ic = open_in filename in
  try 
    let lexbuf = Lexing.from_channel ic in
    let p = Parser.prog Lexer.token lexbuf in 
    close_in ic;
    p
  with Parseutils.Parse_Exception(s,pos) -> 
    (close_in ic; Parseutils.error_exit pos s) ;;


let _ =
  match Array.to_list Sys.argv with
  | _::l -> let bcs = List.map parse l in 
            let labels,functions = Collect.prog (List.concat bcs) in
            (* List.iter (fun ((m,f),(_,_)) -> Printf.printf "(---->%s.%s\n" m f) functions; *)
            
            let kbcs = Bc2kbc.prog bcs labels functions in
            let kbc = List.concat kbcs in
            let start,n = match !Bc2kbc.main_main_pos with
              | None -> failwith "main.main"
              | Some (start,n) -> (start,n) in
            
            (* Printf.printf "%s\n" (String.concat "\n" (List.mapi (fun i ins -> Printf.sprintf "%d | %s" i (Kbc.string_of_instr ins)) kbc));
            Printf.printf "-->%d\n" start; *)
            Interp.interp start n (Array.of_list kbc)
  | _ -> exit 0
