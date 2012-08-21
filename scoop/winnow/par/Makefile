all: main
	./main -i main.in > main.out
	diff main.out main.gold
	./main -i main2.in > main2.out
	diff main2.out main2.gold
	./main -i main3.in > main3.out
	diff main3.out main3.gold
	./main -i main4.in > main4.out
	diff main4.out main4.gold

main: main.e tuple_comparator.e tuple_sorter.e \
				parfor_aggregator.e parfor_worker.e parfor_reader.e
	ec main.e

