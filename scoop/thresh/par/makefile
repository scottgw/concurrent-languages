all: main
	./main -i main.in
	./main -i main2.in
	./main -i main3.in
	./main -i main4.in
	./main -i main5.in
	./main -i main6.in
	#./main -i main7.in

main: main.e \
				reduce2d_worker.e reduce2d_aggregator.e reduce2d_reader.e \
				reduce2d_operator.e \
				parfor_worker.e parfor_aggregator.e parfor_reader.e
	ec main.e
