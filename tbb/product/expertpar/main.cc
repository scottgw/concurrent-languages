/* product: matrix-vector product
 *
 * input:
 *   matrix: a real matrix
 *   vec: a real vector
 *   nelts: the number of elements
 *
 * output:
 *   result: a real vector, whose values are the result of the product
 */
#include <cstdio>
#include <cstring>

#include <iostream>
#include <vector>

#include "tbb/blocked_range.h"
#include "tbb/parallel_for.h"
#include "tbb/task_scheduler_init.h"


using namespace std;
using namespace tbb;

static int is_bench = 0;
int n_threads = task_scheduler_init::default_num_threads();

static double matrix[10000][10000];
static double vec[10000];
static double result[10000];

typedef blocked_range<size_t> range;

void product(int nelts) {
  parallel_for(
    range(0, nelts),
    [&](range r) {
      for (size_t i = r.begin(); i != r.end(); ++i) {
        double sum = 0;
        for (int j = 0; j < nelts; j++) {
          sum += matrix[i][j] * vec[j];
        }
        result[i] = sum;
      }
  });
}

int main(int argc, char** argv) {
  int nelts;

  for (int i = 1; i < argc; i++) {
    if (!strcmp(argv[i], "--is_bench")) {
      is_bench = 1;
    } else if (!strcmp(argv[i], "--threads")) {
      sscanf(argv[i + 1], "%d", &n_threads);
      i++;
    }
  }

  task_scheduler_init init(n_threads);

  scanf("%d", &nelts);

  if (!is_bench) {
    for (int i = 0; i < nelts; i++) {
      for (int j = 0; j < nelts; j++) {
        scanf("%f", &matrix[i][j]);
      }
    }

    for (int i = 0; i < nelts; i++) {
      scanf("%f", &vec[i]);
    }
  }

  product(nelts);

  if (!is_bench) {
    printf("%d\n", nelts, nelts);
    for (int i = 0; i < nelts; i++) {
      printf("%g ", result[i]);
    }
    printf("\n");
  }

  return 0;
}
