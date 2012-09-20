all: main
	./main -N 5 -S 666 -P 50 -WN 6 > main.out; diff -Nuar main.out main.gold

main: main.go
	go build -o main
