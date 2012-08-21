all: main
	./main < main.in > main.out
	diff main.out main.gold
	./main --nproc 4 < main.in
	./main --is_bench < main.in

# flags: --cilk-profile, --cilk-span
main: main.cilk randmat.cilk thresh.cilk winnow.cilk outer.cilk product.cilk
	cilkc -O2 -o main $^
