SIZE=700

all: cluster.png
	eog $< &

%.png: %.ppm
	mogrify -reverse -flatten $<
	mogrify -resize ${SIZE}x${SIZE} -format png $<
%.ppm: %.perf
	./bargraph.pl -fig $< | fig2dev -L ppm -m 4 > $@
