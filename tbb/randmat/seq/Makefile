CXXFLAGS+=-std=c++0x -O2 -m64 -L/home/ensi3a/soaresdk/tbb40_20120408oss/examples/../build/linux_intel64_gcc_cc4.1.2_libc2.5_kernel2.6.18_release -L/home/ensi3a/soaresdk/tbb40_20120408oss/examples/../build/linux_intel64_gcc_cc4.1.2_libc2.5_kernel2.6.18_debug -ltbb -lpthread -lrt -lrt
all: main
	echo "10 9 8" | ./main
	echo "1 4 8" | ./main
	echo "1 5 8" | ./main
	echo "2 5 8" | ./main
	echo "2 5 9" | ./main
	echo "2 5 9" | ./main --is_bench

CXXFLAGS+=-std=c++0x -O2
LDFLAGS+=-ltbb
CXX=g++-4.5
