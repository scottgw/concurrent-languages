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

#include "tbb/tbb.h"

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
    [&, nelts](range r) {
      auto r_end = r.end();
      for (size_t i = r.begin(); i != r_end; ++i) {
        int j = 0;
        double sum = 0;

        for (; (nelts - j) & 3; ++j)
          sum  += matrix [i][j]     * vec [j];

        double acc1, acc2, acc3, acc4;
        acc1 = acc2 = acc3 = acc4 = 0;

        for (; j < nelts; j += 4) {
          acc1 += matrix [i][j]     * vec [j];
          acc2 += matrix [i][j + 1] * vec [j + 1];
          acc3 += matrix [i][j + 2] * vec [j + 2];
          acc4 += matrix [i][j + 3] * vec [j + 3];
        }

        sum += acc1 + acc2 + acc3 + acc4;

        result [i] = sum;
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

  cin >> nelts;

  if (!is_bench) {
    for (int i = 0; i < nelts; i++) {
      for (int j = 0; j < nelts; j++) {
        cin >> matrix[i][j];
      }
    }

    for (int i = 0; i < nelts; i++) {
      cin >> vec[i];
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
