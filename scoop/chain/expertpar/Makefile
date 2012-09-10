all: main
	./main -i main.in

main: main.e worker.e tuple_sorter.e tuple_comparator.e
	ec -config main.ecf -target main -finalize -c_compile

