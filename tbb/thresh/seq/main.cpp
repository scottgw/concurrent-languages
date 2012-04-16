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

void randmat(int nrows, int ncols, int s, int** matrix) {
  srand(s);
  for (int i = 0; i < nrows; i++) {
    for (int j = 0; j < ncols; j++) {
      matrix[i][j] = rand();
    }
  }
}

int main(int argc, char** argv) {
  int nrows, ncols, s;

  scanf("%d%d%d", &nrows, &ncols, &s);

  int** matrix = new int* [nrows];
  for (int i = 0; i < nrows; i++) {
    matrix[i] = new int[ncols];
  }

  randmat(nrows, ncols, s, matrix);

  printf("%d %d\n", nrows, ncols);
  for (int i = 0; i < nrows; i++) {
    for (int j = 0; j < ncols; j++) {
      printf("%d ", matrix[i][j]);
    }
    printf("\n");
  }
  printf("\n");

  for (int i = 0; i < nrows; i++) {
    delete[] matrix[i];
  }
  delete[] matrix;

  return 0;
}
