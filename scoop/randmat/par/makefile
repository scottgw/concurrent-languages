SRC =  main.e randmat_parfor_worker.e randmat_parfor_aggregator.e

all: main
	./main -i main.in
	./main -i main2.in
	./main -i main3.in
	./main -i main4.in
	./main -i main5.in
	./main -i main.in -e is_bench

main: main.e $(SRC)
	ec main.e

