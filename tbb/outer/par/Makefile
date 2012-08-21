all: main
	./$< < $<.in > main.out --threads 4
	diff main.out main.gold
	./$< < $<.in --threads 4
	./$< < $<.in --threads 4 --is_bench

CXXFLAGS+=-std=c++0x -O2 -m64 -L/home/ensi3a/soaresdk/tbb40_20120408oss/examples/../build/linux_intel64_gcc_cc4.1.2_libc2.5_kernel2.6.18_release -L/home/ensi3a/soaresdk/tbb40_20120408oss/examples/../build/linux_intel64_gcc_cc4.1.2_libc2.5_kernel2.6.18_debug -ltbb -lpthread -lrt -lrt
LDFLAGS+=-ltbb
LDFLAGS+=-ltbb
CXX=g++-4.5
