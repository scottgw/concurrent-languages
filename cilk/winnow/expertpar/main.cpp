/*
 * winnow: weighted point selection
 *
 * input:
 *   matrix: an integer matrix, whose values are used as masses
 *   mask: a boolean matrix showing which points are eligible for
 *     consideration
 *   nrows, ncols: the number of rows and columns
 *   nelts: the number of points to select
 *
 * output:
 *   points: a vector of (x, y) points
 */

#include <cilk/cilk.h>
#include <cilk/reducer_opadd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <algorithm>

using namespace std;

static int is_bench = 0;

static unsigned char matrix[20000][20000];
static unsigned char mask[20000][20000];
static int count_per_line[20001];

typedef struct sPoint {
  int value, i, j;
} Point;


static pair <int, pair <int, int> > points[20000];
static pair <int, pair <int, int> > values [20000];

int compare(const Point* vl, const Point* vr) {
  const Point* l = vl, *r = vr;
  return (l->value - r->value);
}

int reduce_sum(int begin, int end, int ncols) {
  int middle = begin + (end - begin) / 2;
  int left, right, res, i;
  if (begin + 1 == end) {
    if (is_bench) {
      for (i = 0; i < ncols; i++) {
        mask[begin][i] = ((begin * i) % (ncols + 1)) == 1;
      }
    }
    res = mask[begin][0];
    for (i = 1; i < ncols; i++) {
      res += mask[begin][i];
    }
    return count_per_line[begin + 1] = res;
  }
  left = cilk_spawn reduce_sum(begin, middle, ncols);
  right = cilk_spawn reduce_sum(middle, end, ncols);
  cilk_sync;
  return left + right;
}

void scan_update_elements(int begin, int end, int* array, int size) {
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
    cilk_spawn scan_update_elements(begin, middle, array, size);
    cilk_spawn scan_update_elements(middle, end, array, size);
  }
}

// Ladner-Fischer
// parallel scan on [begin, end)
void scan_impl(int begin, int end, int* array, int size) {
  if (end - begin > size) {
    cilk_spawn scan_update_elements(begin, end, array, size);
    cilk_sync;
    cilk_spawn scan_impl(begin + size, end, array, 2 * size);
    cilk_sync;
    cilk_spawn scan_update_elements(begin + size, end, array, size);
  }
}

void scan(int n, int* array) {
  cilk_spawn scan_impl(0, n, array, 1);
}

void prefix_sum(int n) {
  cilk_spawn scan(n, count_per_line);
}

void fill_values(int begin, int end, int ncols) {
  int middle = begin + (end - begin) / 2;
  int count, j;
  if (begin + 1 == end) {
    count = count_per_line[begin];
    for (j = 0; j < ncols; j++) {
      if (mask[begin][j] == 1) {
        values[count].first = matrix[begin][j];
        values[count].second.first = begin;
        values[count].second.second = j;
        count++;
      }
    }
    return;
  }
  cilk_spawn fill_values(begin, middle, ncols);
  cilk_spawn fill_values(middle, end, ncols);
}

void winnow(int nrows, int ncols, int nelts) {
  int i, n =  0, chunk, index;

  n = cilk_spawn reduce_sum(0, nrows, ncols);
  cilk_sync;

  cilk_spawn prefix_sum(nrows + 1);
  cilk_sync;

  cilk_spawn fill_values(0, nrows, ncols);
  cilk_sync;

  sort(values, values + n);

  chunk = n / nelts;

  for (i = 0; i < nelts; i++) {
    index = i * chunk;
    points[i] = values[index];
  }
}

void read_matrix(int nrows, int ncols) {
  int i, j;
  for (i =  0; i < nrows; i++) {
    for (j = 0; j < ncols; j++) {
      scanf("%hhu", &matrix[i][j]);
    }
  }
}

void read_mask(int nrows, int ncols) {
  int i, j;
  for (i =  0; i < nrows; i++) {
    for (j = 0; j < ncols; j++) {
      scanf("%hhu", &mask[i][j]);
    }
  }
}

int main(int argc, char *argv[]) {
  int nrows, ncols, nelts, i;

  if (argc == 2) {
    if (!strcmp(argv[argc - 1], "--is_bench")) {
      is_bench = 1;
    }
  }

  scanf("%d%d", &nrows, &ncols);

  if (!is_bench) {
    read_matrix(nrows, ncols);
    read_mask(nrows, ncols);
  }

  scanf("%d", &nelts);

  cilk_spawn winnow(nrows, ncols, nelts);
  cilk_sync;

  if (!is_bench) {
    printf("%d\n", nelts);
    for (i = 0; i < nelts; i++) {
      printf("%d %d\n", points[i].second.first, points[i].second.second);
    }
    printf("\n");
  }

  return 0;
}
