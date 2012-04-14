all: main
	./main < main.in
	echo "1 4 8" | ./main
	echo "1 5 8" | ./main
	echo "2 5 8" | ./main
	echo "2 5 9" | ./main

main: main.cilk
	cilkc -o main main.cilk
