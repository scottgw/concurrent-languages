/*
 * outer: outer product
 *
 * input:
 *   winnow_points: a vector of (x, y) points
 *   nelts: the number of points
 *
 * output:
 *   outer_matrix: a real matrix, whose values are filled with inter-point
 *     distances
 *   outer_vector: a real vector, whose values are filled with origin-to-point
 *     distances
 */

#include <cilk-lib.cilkh>
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

double outer_matrix[10000][10000];
double outer_vector[10000];

typedef struct sPoint {
  int i, j;
} Point;

extern Point winnow_points[10000];

double sqr(double x) {
  return x * x;
}

double distance(Point a, Point b) {
  return sqrt(sqr(a.i - b.i) + sqr(a.j - b.j));
}

static double max(double a, double b) {
  return a > b ? a : b;
}

// parallel for on [begin, end)
static cilk void fill_matrix(int begin, int end, int ncols) {
  int middle = begin + (end - begin) / 2;
  double nmax = -1;
  int j;
  if (begin + 1 == end) {
    for (j = 0; j < ncols; j++) {
      if (begin != j) {
        outer_matrix[begin][j] = distance(winnow_points[begin], winnow_points[j]);
        nmax = max(nmax, outer_matrix[begin][j]);
      }
      outer_matrix[begin][begin] = nmax * ncols;
      outer_vector[begin] = distance((Point){0, 0}, winnow_points[begin]);
    }
    return;
  }
  spawn fill_matrix(begin, middle, ncols);
  spawn fill_matrix(middle, end, ncols);
}

cilk void outer(int nelts) {
  spawn fill_matrix(0, nelts, nelts);
}
