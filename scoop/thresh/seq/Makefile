all: main
	./main -i main.in
	./main -i main2.in
	./main -i main3.in
	./main -i main4.in
	./main -i main5.in
	./main -i main6.in

main: main.e
	ec -config main.ecf -target main -finalize -c_compile;
	cp EIFGENs/main/F_code/main .