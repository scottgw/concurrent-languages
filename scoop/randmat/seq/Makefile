all: main
	./main -i main.in
	./main -i main2.in
	./main -i main3.in
	./main -i main4.in
	./main -i main5.in
	./main -i main5.in -is_bench 

main: main.e
	ec -config main.ecf -target main -finalize -c_compile;
	cp EIFGENs/main/F_code/main .

allw: mainw
	./mainw -i main.in
	./mainw -i main2.in
	./mainw -i main3.in
	./mainw -i main4.in
	./mainw -i main5.in
	./mainw -i main5.in -is_bench 

mainw: main.e
	ec -config main.ecf -target main -c_compile;
	cp EIFGENs/main/W_code/main mainw

