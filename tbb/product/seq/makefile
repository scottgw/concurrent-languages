all: main
	./$< < $<.in > main.out
	diff main.out main.gold

CXXFLAGS+=-std=c++0x
LDFLAGS+=-ltbb
CXX=g++-4.5
