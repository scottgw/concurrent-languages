all: main
	./main < main.in
	./main < main2.in
	./main < main3.in
	./main < main4.in

main: main.chpl
	chpl main.chpl -o main
