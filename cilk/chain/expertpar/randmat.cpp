/*
 * randmat: random number generation
 *
 * input:
 *   nrows, ncols: the number of rows and columns
 *   s: the seed
 *
 * output:
 *   randmat_matrix: an nrows x ncols integer matrix
 */

#include <cilk-lib.cilkh>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

unsigned char randmat_matrix[20000][20000];

cilk void fill_row(int begin, int ncols, int seed) {
  int j;
  for (j = 0; j < ncols; j++) {
    randmat_matrix[begin][j] = (unsigned char) (rand_r(&seed) % 100);
  }
}

// parallel for on [begin, end), calling f()
cilk void fill_matrix(int begin, int end, int ncols, int seed) {
  int middle = begin + (end - begin) / 2;
  if (begin + 1 == end) {
    spawn fill_row(begin, ncols, seed + begin);
  } else {
    spawn fill_matrix(begin, middle, ncols, seed);
    spawn fill_matrix(middle, end, ncols, seed);
  }
}

cilk void randmat(int nrows, int ncols, int s) {
  // parallel for on rows
  spawn fill_matrix(0, nrows, ncols, s);
}
