all: main
	./main < main.in > main.out
	./main < main2.in > main2.out
	./main < main3.in > main3.out
	./main < main4.in > main4.out
	diff main.out main.g
	diff main2.out main2.g
	diff main3.out main3.g
	diff main4.out main4.g
	./main < main.in
	./main --is_bench < main.in

main: main.chpl
	chpl main.chpl -o main
