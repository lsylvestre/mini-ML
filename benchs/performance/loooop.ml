let i = ref 0 in
for j = 0 to 10000 do
	i := 0;
	while (!i < 10000) do
	  i := !i + 1
	done
done;
print_int (!i)