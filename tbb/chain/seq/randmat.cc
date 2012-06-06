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
#include <cstring>

unsigned char randmat_matrix[30000][30000];

void randmat(int nrows, int ncols, unsigned int seed) {
  const int LCG_A = 1664525, LCG_C = 1013904223;
  for (int i = 0; i < nrows; i++) {
    for (int j = 0; j < ncols; j++) {
      randmat_matrix[i][j] = seed = (LCG_A * seed + LCG_C) % 100;
    }
  }
}
