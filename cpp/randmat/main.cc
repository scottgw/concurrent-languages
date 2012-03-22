/* randmat: random number generator
 *
 * input:
 *   nrows, ncols: number of rows and columns
 *   s: random number generation seed
 *
 * output:
 *   matrix: random nrows x ncols integer matrix
 */
#include <cassert>
#include <cstdio>
#include <cstdlib>

// TODO: function randmat

int main(int argc, char** argv) {
  assert(argc == 4);
  int nrows = atoi(argv[1]),
      ncols = atoi(argv[2]),
      s = atoi(argv[3]);
  srand(s);
  printf("%d %d\n", nrows, ncols);
  for (int i = 0; i < nrows; i++) {
    for (int j = 0; j < ncols; j++) {
      printf("%d ", rand());
    }
    printf("\n");
  }
  printf("\n");
  return 0;
}
