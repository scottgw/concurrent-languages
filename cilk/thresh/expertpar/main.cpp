/*
 * thresh: histogram thresholding
 *
 * input:
 *   matrix: the integer matrix to be thresholded
 *   nrows, ncols: the number of rows and columns
 *   percent: the percentage of cells to retain
 *
 * output:
 *   mask: a boolean matrix filled with true for cells kept
 */

#include <cilk/cilk.h>
#include <cilk/reducer_max.h>
#include <cilk/cilk_api.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

int is_bench = 0;
static unsigned char matrix[20000][20000];
static unsigned char mask[20000][20000];
static int histogram[16][200];

int reduce_max (int nrows, int ncols) {
  cilk::reducer_max <int> max_reducer (0);

  cilk_for (int i = 0; i < nrows; i++) {
    int begin = i;

    for (int j = 0; j < ncols; j++) {
      max_reducer.calc_max (matrix [begin][j]);
    }
  }

  return max_reducer.get_value ();
}

void fill_histogram(int nrows, int ncols) {
  int P = __cilkrts_get_nworkers();
  cilk_for (int r = 0; r < nrows; ++r) {
    int Self = __cilkrts_get_worker_number();
    for (int i = 0; i < ncols; i++) {
      histogram [Self][matrix[r][i]]++;
    }
  }
}

void merge_histogram () {
  int P = __cilkrts_get_nworkers();
  cilk_for (int v = 0; v < 100; ++v) {
    int merge_val = __sec_reduce_add (histogram [1:(P-1)][v]);
    histogram [0][v] += merge_val;
  }
}

void fill_mask (int nrows, int ncols, int threshold) {
  cilk_for (int i = 0; i < nrows; ++i) {
    for (int j = 0; j < ncols; ++j) {
      mask[i][j] = matrix [i][j] >= threshold;
    }
  }
}

void thresh(int nrows, int ncols, int percent) {
  int i;
  int nmax = 0;
  int count, prefixsum, threshold;

  nmax = reduce_max(nrows, ncols);

  fill_histogram(nrows, ncols);
  merge_histogram();

  count = (nrows * ncols * percent) / 100;

  prefixsum = 0;
  threshold = nmax;

  for (i = nmax; i >= 0 && prefixsum <= count; i--) {
    prefixsum += histogram[0][i];
    threshold = i;
  }

  fill_mask(nrows, ncols, threshold);
}

int main(int argc, char *argv[]) {
  int nrows, ncols, percent, i, j;

  if (argc >= 2) {
    for (int a = 0; a < argc; a++){
      if (!strcmp(argv[a], "--is_bench")) {
        is_bench = 1;
      }
    }
  }

  scanf("%d%d", &nrows, &ncols);

  if (!is_bench) {
    for (i = 0; i < nrows; i++) {
      for (j = 0; j < ncols; j++) {
        scanf("%hhu", &matrix[i][j]);
      }
    }
  }

  scanf("%d", &percent);

  thresh(nrows, ncols, percent);

  if (!is_bench) {
    printf("%d %d\n", nrows, ncols);
    for (i = 0; i < nrows; i++) {
      for (j = 0; j < ncols; j++) {
        printf("%hhu ", mask[i][j]);
      }
      printf("\n");
    }
    printf("\n");
  }

  return 0;
}
