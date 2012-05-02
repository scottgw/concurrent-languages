all: main
	./main < main.in > main.out
	diff main.out main.gold

main: main.cilk
	cilkc -o main main.cilk
