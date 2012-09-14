all: main
	./main < main.in > main.out
	diff main.out main.gold
	./main < main.in
	./main --is_bench < main.in

main: main.chpl
	chpl -O main.chpl -o main
