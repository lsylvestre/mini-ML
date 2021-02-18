(**************************************************************************)
(*                                                                        *)
(*         PSTL : OCaml sur la plate-forme Nand2Tetris (2020)             *)
(*                                                                        *)      
(*           Loïc SYLVESTRE              Pablito BELLO                    *)
(*           loic.sylvestre@etu.upmc.fr  pablito.bello@etu.upmc.fr        *)
(*                                                                        *)  
(**************************************************************************)

let add_long n =
  let p = !Domain.data_top in
  Domain.data.(p) <- Mlvalues.val_long n;
  incr Domain.data_top;
  Mlvalues.val_long p

let alloc tag sz =
  let p = !Domain.data_top in
  Domain.data.(p) <- Block.make_header tag sz;
  Domain.data_top := p + sz + 1;
  p

let set_data addr i v =
  Domain.data.(addr+i+1) <- v

let add_string s =
  let z = String.length s in
  let p = alloc Block.string_tag z in
  for i = 0 to z - 1 do
    set_data p i (Mlvalues.val_long @@ Obj.magic @@ String.get s i)
  done;
  Mlvalues.val_ptr p

let add_unknown () = add_long 0

let push_global v = 
  Domain.global.(!Domain.global_top) <- v;
  incr Domain.global_top
