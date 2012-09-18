/*
 * thresh: histogram thresholding
 *
 * input:
 *   randmat_matrix: the integer matrix to be thresholded
 *   nrows, ncols: the number of rows and columns
 *   percent: the percentage of cells to retain
 *
 * output:
 *   thresh_mask: a boolean matrix filled with true for cells kept
 */

#include <cilk-lib.cilkh>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

extern unsigned char randmat_matrix[20000][20000];
unsigned char thresh_mask[20000][20000];
static int histogram[16][100];

int max(int a, int b) {
  return a > b ? a : b;
}

// parallel max reduce on [begin, end)
cilk int reduce_max(int begin, int end, int ncols) {
  int middle = begin + (end - begin) / 2;
  int left, right, res, i;
  if (begin + 1 == end) {
    res = randmat_matrix[begin][0];
    for (i = 1; i < ncols; i++) {
      res = max(res, randmat_matrix[begin][i]);
    }
    return res;
  }
  left = spawn reduce_max(begin, middle, ncols);
  right = spawn reduce_max(middle, end, ncols);
  sync;
  return max(left, right);
}

cilk void fill_histogram(int begin, int end, int ncols) {
  int middle = begin + (end - begin) / 2;
  int i;
  if (begin + 1 == end) {
    for (i = 0; i < ncols; i++) {
      histogram[Self][randmat_matrix[begin][i]]++;
    }
    return;
  }
  spawn fill_histogram(begin, middle, ncols);
  spawn fill_histogram(middle, end, ncols);
  sync;
}

cilk void merge_histogram(int begin, int end) {
  int middle = begin + (end - begin) / 2;
  int i;
  if (begin + 1 == end) {
    for (i = 1; i < Cilk_active_size; i++) {
      histogram[0][begin] += histogram[i][begin];
    }
    return;
  }
  spawn merge_histogram(begin, middle);
  spawn merge_histogram(middle, end);
  sync;
}

cilk void fill_mask(int begin, int end, int ncols, int threshold) {
  int middle = begin + (end - begin) / 2;
  int i;
  if (begin + 1 == end) {
    for (i = 0; i < ncols; i++) {
      thresh_mask[begin][i] = randmat_matrix[begin][i] >= threshold;
    }
    return;
  }
  spawn fill_mask(begin, middle, ncols, threshold);
  spawn fill_mask(middle, end, ncols, threshold);
  sync;
}

cilk void thresh(int nrows, int ncols, int percent) {
  int i;
  int nmax = 0;
  int count, prefixsum, threshold;

  nmax = spawn reduce_max(0, nrows, ncols);
  sync;

  spawn fill_histogram(0, nrows, ncols);
  sync;
  spawn merge_histogram(0, nmax + 1);
  sync;

  count = (nrows * ncols * percent) / 100;

  prefixsum = 0;
  threshold = nmax;

  for (i = nmax; i >= 0 && prefixsum <= count; i--) {
    prefixsum += histogram[0][i];
    threshold = i;
  }

  spawn fill_mask(0, nrows, ncols, threshold);
  sync;
}
