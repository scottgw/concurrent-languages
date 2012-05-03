all: main
	./main < main.in

main: main.8 randmat.8 thresh.8 winnow.8 outer.8 product.8
	8l -o main main.8

%.8: %.go
	8g $<
