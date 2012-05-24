all: main
	echo "10 9 8" | ./main
	echo "1 4 8" | ./main
	echo "1 5 8" | ./main
	echo "2 5 8" | ./main
	echo "2 5 9" | ./main

main: main.8
	8l -o main main.8

main.8: main.go
	8g -O2 main.go
