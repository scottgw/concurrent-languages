all: main
	./main < main.in

# flags: --cilk-profile, --cilk-span
main: main.cilk randmat.cilk thresh.cilk winnow.cilk outer.cilk product.cilk
	cilkc -o main $^
