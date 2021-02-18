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
  match Sys.argv with
  | [|_;filename|] -> let p = parse filename in 
                      Printf.printf "%s\n" (Ast.string_prog p);
                      Typing.w p;
                      let r = Compile.main p in
                      let oc = open_out "generated_files/Main.vm" in
                      output_string oc r;
                      close_out oc
  | _ -> exit 0
