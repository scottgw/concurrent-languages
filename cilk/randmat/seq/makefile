all: main
	./main < main.in

main: main.cilk
	cilkc -o main main.cilk
