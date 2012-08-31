all: main
	./main -i main.in

main: main.e randmat.e thresh.e winnow.e outer.e product.e
	ec -config main.ecf -target main -finalize -c_compile

