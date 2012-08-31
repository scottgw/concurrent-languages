all: main
	./main -i main.in

main: main.e randmat.e thresh.e winnow.e outer.e product.e
	ec -config main.ecf -target main -finalize -c_compile;
	cp EIFGENs/main/W_code/main .

allw: mainw
	./mainw -i main.in

mainw: main.e randmat.e thresh.e winnow.e outer.e product.e
	ec -config main.ecf -target main -c_compile;
	cp EIFGENs/main/W_code/main mainw

