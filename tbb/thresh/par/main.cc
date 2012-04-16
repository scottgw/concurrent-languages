/* thresh: histogram thresholding
 *
 * input:
 *   matrix: the integer matrix to be thresholded
 *   nrows, ncols: number of rows and columns
 *   percent: the percentage of cells to retain
 *
 * output:
 *   mask: a boolean matrix filled with true for cells that are kept
 */
#include <cassert>
#include <cstdio>
#include <cstdlib>

#include <algorithm>

#include "tbb/parallel_reduce.h"
#include "tbb/blocked_range.h"

using namespace std;

#include <thread>

void func() {}

int max(int a, int b) {
  std::thread t(func);
  t.join();
  return a > b ? a : b;
}

typedef int (*Operator)(int acc, int x);

int reduce2d(int nrows, int ncols, int** matrix, Operator op) {
  return tbb::parallel_reduce(
      tbb::blocked_range<const int*>(matrix[0], matrix[0] + ncols),
      0,
      [&](tbb::blocked_range<const int*> r, int partial_value)->float {
        return std::accumulate(r.begin(), e.end(), partial_value)
      },
      std::plus<int>());
}

void thresh(int nrows, int ncols, int** matrix, int percent, int** mask) {
  int nmax = 0;
  //for (int i = 0; i < nrows; i++) {
    //for (int j = 0; j < ncols; j++) {
      //nmax = max(nmax, matrix[i][j]);
    //}
  //}
  
  nmax = reduce2d(nrows, ncols, matrix, max);

  int* histogram = new int[nmax + 1];

  for (int i = 0; i < nrows; i++) {
    for (int j = 0; j < ncols; j++) {
      histogram[matrix[i][j]]++;
    }
  }

  int count = (nrows * ncols * percent) / 100;

  int prefixsum = 0;
  int threshold = nmax;

  for (int i = nmax; i >= 0 && prefixsum <= count; i--) {
    prefixsum += histogram[i];
    threshold = i;
  }

  for (int i = 0; i < nrows; i++) {
    for (int j = 0; j < ncols; j++) {
      mask[i][j] = matrix[i][j] >= threshold;
    }
  }

  delete[] histogram;
}

int main(int argc, char** argv) {
  int nrows, ncols, percent;

  scanf("%d%d", &nrows, &ncols);

  int** matrix = new int* [nrows];
  for (int i = 0; i < nrows; i++) {
    matrix[i] = new int[ncols];
  }

  int** mask = new int* [nrows];
  for (int i = 0; i < nrows; i++) {
    mask[i] = new int[ncols];
  }

  for (int i = 0; i < nrows; i++) {
    for (int j = 0; j < ncols; j++) {
      scanf("%d", &matrix[i][j]);
    }
  }

  scanf("%d", &percent);

  thresh(nrows, ncols, matrix, percent, mask);

  printf("%d %d\n", nrows, ncols);
  for (int i = 0; i < nrows; i++) {
    for (int j = 0; j < ncols; j++) {
      printf("%d ", mask[i][j]);
    }
    printf("\n");
  }
  printf("\n");

  for (int i = 0; i < nrows; i++) {
    delete[] matrix[i];
  }
  delete[] matrix;

  for (int i = 0; i < nrows; i++) {
    delete[] mask[i];
  }
  delete[] mask;

  return 0;
}
