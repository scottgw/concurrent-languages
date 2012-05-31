all: main
	./main < main.in
	./main --is_bench < main.in

main: main.chpl
	chpl main.chpl -o main
