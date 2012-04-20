all: main
	./main -i main.in
	./main -i main2.in
	./main -i main3.in
	./main -i main4.in
	./main -i main5.in
	./main -i main6.in
	#./main -i main7.in

main: main.e reduce2d_worker.e reduce2d_aggregator.e
	ec main.e
