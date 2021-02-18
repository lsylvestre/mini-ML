(**************************************************************************)
(*                                                                        *)
(*         PSTL : OCaml sur la plate-forme Nand2Tetris (2020)             *)
(*                                                                        *)      
(*           Loïc SYLVESTRE              Pablito BELLO                    *)
(*           loic.sylvestre@etu.upmc.fr  pablito.bello@etu.upmc.fr        *)
(*                                                                        *)  
(**************************************************************************)

let debug = false

(* ALLOC fonctions *)

let heap_can_alloc size =
  !Domain.heap_top + size < !Domain.heap_size


let next = ref 0 (* premiere position disponible dans to_space lors de la copie *)

(* libération des semi-spaces lors du redimensionnement  *) 
(* implantation en OCaml : la fonction free ne fait rien 
   (le gc se charge de libérer les objets) *)
(* implantation en mini-ml la fonction free libère le pointeur passé en argument *)
let free a = Mlvalues.free a 

let resize_spaces size =
  (* on traite le redimensionnement des semi spaces si nécessaire *)
  let half = !Domain.heap_size / 2 in (* nombre d'éléments à la moitié d'un semi space *)
  let quarter = half / 2 in (* nombre d'élements au quart d'un semi space *)
  (* définition de la nouvelle taille *)
  let old_size = !Domain.heap_size in
  (* si il n'y a pas assez de place pour l'allocation
     on redimensionne en rajoutant a la taille la place de l'allocation
     puis multiplie le tout par deux *)
  if !Domain.heap_top + size >= !Domain.heap_size then
    Domain.heap_size := (!Domain.heap_size + size) * 2
  else 
    begin
      (* si taille + allocation < 25% de la taille de base, 
         on diminue la taille par deux *)
      if quarter > (!Domain.heap_top + size) then (* si remplis à moins de 25% *)
        Domain.heap_size := !Domain.heap_size / 2
    end;
  (* si la taille à changée *)
  if old_size <> !Domain.heap_size then
    begin
      if debug then
        begin
          print_string "resize spaces, old size : ";
          print_int old_size;
          print_string ", new size : ";
          print_int !Domain.heap_size;
          print_newline ()
        end;
      (* création du nouveau from_space à la bonne taille *)
      let new_from_space = Array.make !Domain.heap_size (Mlvalues.val_long 0) in
      (* copie de l'ancien from_space dans le nouveau *)
      for i = 0 to !Domain.heap_top - 1 do
        new_from_space.(i) <- (!Domain.from_space).(i)
      done;
      free !Domain.from_space;
      Domain.from_space := new_from_space;
      free !Domain.to_space;
      Domain.to_space := Array.make !Domain.heap_size (Mlvalues.val_long 0) 
    end

(* Traite le déplacement d'un bloc de to_space vers from_space si nécéssaire,   *)
(* sinon suit le fwd_ptr                                                        *)
(* value est le mlvalue pointeur vers le bloc,                                  *)
(* is_array dis si la source est un tableau ou pas : true si pile/tas , faux si env/acc *)
(* source_reg est le registre qui contient value si is_array est false (acc, env)  *)
(* source_arr est le tableau contenant value a la pos pos_arr si is_array est true (pile, tas) *)

let move_addr value is_array source_reg source_arr pos_arr =
  if Mlvalues.is_ptr value then (* val pointe vers un bloc *)
    begin
      if Mlvalues.ptr_val value < Domain.global_start
         || Block.tag (Mlvalues.ptr_val value) >= Block.no_scan_tag 
      then () (* c'est une valeur dans le segment data, 
                 qui ne pointe vers aucune valeur allouée *)
      else if Block.tag (Mlvalues.ptr_val value) = Block.fwd_ptr_tag
      then (* le bloc pointé est un fwd_ptr *)
        (* on fait pointer value sur la nouvelle destination *)
        begin
          if is_array then source_arr.(pos_arr) <- Block.get_field value 0
          else source_reg := Block.get_field value 0
        end
      else (* le bloc n'a pas été déplacé, on le copie *)
        begin
          let old = !next in (* sauvegarde de l'endroit où on va copier dans to_space *)
          (* on copie tout le bloc, header compris dans to_space *)
          (!Domain.to_space).(old) <- Block.get_field value (-1); (* copie le header *)
          for j = 0 to (Block.size (Mlvalues.ptr_val value)) - 1 do
            (* copie tout les fields *)
            (!Domain.to_space).(old + j + 1) <- Block.get_field value j
          done;
          next := !next + (Block.size (Mlvalues.ptr_val value)) + 1;
          (* prochaine pos dispo dans to_space *)
          (* on change le tag du bloc en fwd_ptr car il a été déplacé  *)
          Block.set_field value (-1)
            (Block.make_header Block.fwd_ptr_tag (Block.size (Mlvalues.ptr_val value)));
          (* ajoute le fwd_ptr dans from_space vers la nouvelle position dans to_space *)
          Block.set_field value 0 (Mlvalues.val_ptr (old + Domain.heap_start)) ;
          (* on fait pointer value vers le nouveau bloc dans to_space *)
          if is_array then source_arr.(pos_arr) <- Mlvalues.val_ptr (old + Domain.heap_start) 
          else source_reg := Mlvalues.val_ptr (old + Domain.heap_start) 
        end
    end

(* lance le gc *)
let run_gc size =
  if debug then print_string "lancement gc\n";
  (* on parcours les éléments de la pile *)

  for i = 0 to !Domain.sp - 1 do
    let value = Domain.stack.(i) in
    move_addr value true (ref (Mlvalues.val_long 0)) Domain.stack i
  done;

  (* on traite l'accu *)
  move_addr !Domain.acc false Domain.acc Domain.stack (-1);
  (* on traite l'env *)
  move_addr !Domain.env false Domain.env Domain.stack (-1);

  (* on parcours les variables globales *)
  for i = 0 to !Domain.global_top - 1 do
    let value = Domain.global.(i) in
    move_addr value true (ref (Mlvalues.val_long 0)) Domain.global i
  done;

  (* maintenant on parcours les fields de tout les objets qu'on a bougé dans to_space *)
  let i = ref 0 in
  while !i < !next do (* parcours les headers *)
    let size = (Mlvalues.long_val (!Domain.from_space).(!i)) / 256 in
    for j = !i + 1 to size do (* parcours les fields du bloc courant *)
      let value = (!Domain.from_space).(!i) in
      move_addr value true (ref (Mlvalues.val_long 0)) !Domain.from_space !i
    done;
    i := !i + size + 1 (* passe au header du bloc suivant dans to_space *)
  done;

  (* on echange from_space et to_space *)
  let tmp = !Domain.to_space in
  Domain.to_space := !Domain.from_space;
  Domain.from_space := tmp;
  Domain.heap_top := !next;
  next := 0;
  (* on redimensionne les espaces si nécéssaire *)
  resize_spaces size;

  if debug then print_string "fin gc";
  print_newline ()


(* Alloue si possible, sinon lance le GC puis alloue *)
let alloc size = 
  if debug then
    begin
      print_newline ();
      print_string "try alloc ";
      print_int size;
      print_newline ()
    end;
  if heap_can_alloc size then
    begin
      if debug then (print_string "can alloc"; print_newline ());
      let ptr = !Domain.heap_top + Domain.heap_start in
      Domain.heap_top := !Domain.heap_top + size;
      ptr  
    end
  else 
    begin
      if debug then 
        begin
          print_string "cannot alloc";
          print_newline ()
        end;
      run_gc size;
      if heap_can_alloc size then 
        begin
          let ptr = !Domain.heap_top + Domain.heap_start  in
          Domain.heap_top := !Domain.heap_top + size;
          ptr  
        end
      else 
        begin
          if debug then 
            begin
              print_string "plus de mémoire";
              print_newline ()
            end; 
          exit 0
        end
    end
