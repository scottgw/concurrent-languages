all: main
	./main < main.in > main.out
	diff main.out main.gold

main: main.chpl
	chpl main.chpl -o main
