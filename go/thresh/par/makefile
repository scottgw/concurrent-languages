all: main
	./main < main.in
	./main < main2.in
	./main < main3.in
	./main < main4.in
	./main < main5.in
	./main < main6.in

main: main.8
	8l -o main main.8

main.8: main.go
	8g main.go
