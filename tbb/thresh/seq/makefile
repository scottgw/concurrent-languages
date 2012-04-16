all: main
	echo "10 9 8" | ./main
	echo "1 4 8" | ./main
	echo "1 5 8" | ./main
	echo "2 5 8" | ./main
	echo "2 5 9" | ./main

main: main.cpp
	gcc -o main main.cpp -ltbb
