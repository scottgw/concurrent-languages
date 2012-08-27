all: main
	./main -i main.in > main.out
	diff main.out main.gold

main: main.e comparator.e
	ec main.e

