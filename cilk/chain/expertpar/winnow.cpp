/*
 * winnow: weighted point selection
 *
 * input:
 *   randmat_matrix: an integer matrix, whose values are used as masses
 *   thresh_mask: a boolean matrix showing which points are eligible for
 *     consideration
 *   nrows, ncols: the number of rows and columns
 *   nelts: the number of points to select
 *
 * output:
 *   winnow_points: a vector of (x, y) points
 */

#include <cilk/cilk.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

extern int is_bench;
extern unsigned char randmat_matrix[20000][20000];
extern unsigned char thresh_mask[20000][20000];
static int count_per_line[20001];

point winnow_points[20000];
static point values[20000];

int compare(const void* vl, const void* vr) {
  const Point* l = vl, *r = vr;
  return (l->value - r->value);
}

cilk int reduce_sum(int begin, int end, int ncols) {
  int middle = begin + (end - begin) / 2;
  int left, right, res, i;
  if (begin + 1 == end) {
    if (is_bench) {
      for (i = 0; i < ncols; i++) {
        thresh_mask[begin][i] = ((begin * i) % (ncols + 1)) == 1;
      }
    }
    res = thresh_mask[begin][0];
    for (i = 1; i < ncols; i++) {
      res += thresh_mask[begin][i];
    }
    return count_per_line[begin + 1] = res;
  }
  left = spawn reduce_sum(begin, middle, ncols);
  right = spawn reduce_sum(middle, end, ncols);
  sync;
  return left + right;
}

cilk void scan_update_elements(int begin, int end, int* array, int size) {
  int middle, count;
  if (end - begin <= size) {
    return;
  } else if (end - begin <= 2 * size) {
    array[begin + size] = array[begin] + array[begin + size];
  } else {
    count = (end - begin) / size;
    count /= 2;
    count += count % 2;  // to ensure it is even
    middle = begin + count * size;
    spawn scan_update_elements(begin, middle, array, size);
    spawn scan_update_elements(middle, end, array, size);
  }
}

// Ladner-Fischer
// parallel scan on [begin, end)
cilk void scan_impl(int begin, int end, int* array, int size) {
  if (end - begin > size) {
    spawn scan_update_elements(begin, end, array, size);
    sync;
    spawn scan_impl(begin + size, end, array, 2 * size);
    sync;
    spawn scan_update_elements(begin + size, end, array, size);
    sync;
  }
}

cilk void scan(int n, int* array) {
  spawn scan_impl(0, n, array, 1);
}

cilk void prefix_sum(int n) {
  spawn scan(n, count_per_line);
}

cilk void fill_values(int begin, int end, int ncols) {
  int middle = begin + (end - begin) / 2;
  int count, j;
  if (begin + 1 == end) {
    count = count_per_line[begin];
    for (j = 0; j < ncols; j++) {
      if (thresh_mask[begin][j] == 1) {
        values[count].value = randmat_matrix[begin][j];
        values[count].i = begin;
        values[count].j = j;
        count++;
      }
    }
    return;
  }
  spawn fill_values(begin, middle, ncols);
  spawn fill_values(middle, end, ncols);
}

cilk void winnow(int nrows, int ncols, int nelts) {
  int i, n =  0, chunk, index;

  n = spawn reduce_sum(0, nrows, ncols);
  sync;

  spawn prefix_sum(nrows + 1);
  sync;

  spawn fill_values(0, nrows, ncols);
  sync;

  qsort(values, n, sizeof(*values), compare);

  chunk = n / nelts;

  for (i = 0; i < nelts; i++) {
    index = i * chunk;
    winnow_points[i] = values[index];
  }
}
