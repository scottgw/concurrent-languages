all: main
	./main < main.in > main.out
	diff main.out main.gold

# flags: --cilk-profile, --cilk-span
main: main.cilk randmat.cilk thresh.cilk winnow.cilk outer.cilk product.cilk
	cilkc -o main $^
