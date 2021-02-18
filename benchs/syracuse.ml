let rec syracuse = function
| 1 -> 1
| n -> if n mod 2 = 0 then syracuse(n/2) else syracuse(3*n+1) ;;

print_int (syracuse 42) ;;
