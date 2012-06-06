all: main
	./$< < $<.in > main.out
	diff main.out main.gold

main: randmat.o thresh.o winnow.o outer.o product.o

CXXFLAGS+=-std=c++0x
LDFLAGS+=-ltbb
CXX=g++-4.5
