all: main
	./main < main.in > main.out
	diff main.out main.gold

main: main.8
	8l -o main main.8

main.8: main.go
	8g main.go
