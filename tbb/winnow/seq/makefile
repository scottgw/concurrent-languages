CXXFLAGS+=-std=c++0x -O2 -m64 -L/home/ensi3a/soaresdk/tbb40_20120408oss/examples/../build/linux_intel64_gcc_cc4.1.2_libc2.5_kernel2.6.18_release -L/home/ensi3a/soaresdk/tbb40_20120408oss/examples/../build/linux_intel64_gcc_cc4.1.2_libc2.5_kernel2.6.18_debug -ltbb -lpthread -lrt -lrt
all: main
	./$< < $<.in > main.out
	diff main.out main.gold
	./$< < $<2.in > main2.out
	diff main2.out main2.gold
	./$< < $<3.in > main3.out
	diff main3.out main3.gold
	./$< < $<4.in > main4.out
	diff main4.out main4.gold
	./$< < $<.in > main.out
	./$< < $<.in
	./$< < $<.in --is_bench

CXXFLAGS+=-std=c++0x -O2
LDFLAGS+=-ltbb
CXX=g++-4.5
