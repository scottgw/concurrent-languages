all: main
	./main < main.in
	./main --is_bench < main.in

main: main.chpl randmat.chpl thresh.chpl winnow.chpl outer.chpl product.chpl
	chpl -O $^ -o main
