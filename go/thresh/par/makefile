all: main
	./main < main.in > main.out
	diff main.out main.gold
	./main < main2.in > main.out
	diff main.out main2.gold
	./main < main3.in > main.out
	diff main.out main3.gold
	./main < main4.in > main.out
	diff main.out main4.gold
	./main < main5.in > main.out
	diff main.out main5.gold
	./main < main6.in > main.out
	diff main.out main6.gold

main: main.8
	8l -o main main.8

main.8: main.go
	8g -O2 main.go
