all: main
	./$< < $<.in > main.out --threads 4
	diff main.out main.gold
	./$< < $<.in --threads 4
	./$< < $<.in --threads 4 --is_bench

CXXFLAGS+=-std=c++0x -O2
LDFLAGS+=-ltbb
CXX=g++-4.5
