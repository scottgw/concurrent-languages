/*
 * outer: outer product
 *
 * input:
 *   points: a vector of (x, y) points
 *   nelts: the number of points
 *
 * output:
 *   matrix: a real matrix, whose values are filled with inter-point
 *     distances
 *   vector: a real vector, whose values are filled with origin-to-point
 *     distances
 */

#include <cilk/cilk.h>
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <algorithm>

using namespace std;

static int is_bench = 0;

static double matrix[10000][10000];
static double vector[10000];

typedef std::pair<int, int> point;
static point points[10000];

double sqr(double x) {
  return x * x;
}

double distance(point a, point b) {
  return sqrt(sqr(a.first - b.first) + sqr(a.second - b.second));
}

// parallel for on [begin, end)
void outer (int nelts) {
  cilk_for (int i = 0; i < nelts; ++i) {
    double nmax = 0;
    for (int j = 0; j < nelts; ++j) {
      if (i != j) {
        matrix [i][j] = ::distance (points [i], points [j]);
        nmax = max (nmax, matrix [i][j]);
      }      
    }
    matrix [i][i] = nmax * nelts;
    vector [i] = ::distance (make_pair (0,0), points [i]);
  }
}


void read_vector_of_points(int nelts) {
  int i, a, b;
  for (i =  0; i < nelts; i++) {
    scanf("%d %d", &a, &b);
    points[i] = make_pair (a, b);
  }
}

int main(int argc, char *argv[]) {
  int nelts, i, j;

  if (argc == 2) {
    if (!strcmp(argv[argc - 1], "--is_bench")) {
      is_bench = 1;
    }
  }

  scanf("%d", &nelts);

  if (!is_bench) {
    read_vector_of_points(nelts);
  }

  outer(nelts);

  if (!is_bench) {
    printf("%d %d\n", nelts, nelts);
    for (i = 0; i < nelts; i++) {
      for (j = 0; j < nelts; j++) {
        printf("%g ", matrix[i][j]);
      }
      printf("\n");
    }
    printf("\n");

    printf("%d\n", nelts);
    for (i = 0; i < nelts; i++) {
      printf("%g ", vector[i]);
    }
    printf("\n");
  }

  return 0;
}
