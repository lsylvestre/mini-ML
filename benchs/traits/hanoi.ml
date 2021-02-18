let mouvement de vers =
    print_string de; 
    print_string "-->";
    print_string vers; 
    print_newline ();;

let rec hanoi depart milieu arrivee = function
| 0 -> ()
| n -> hanoi depart arrivee milieu (n - 1); 
       mouvement depart arrivee;
       hanoi milieu depart arrivee (n - 1);;

mouvement "A" "B" ;;
hanoi "A" "B" "C" 1 ;;