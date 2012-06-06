/* thresh: histogram thresholding
 *
 * input:
 *   randmat_matrix: the integer matrix to be thresholded
 *   nrows, ncols: number of rows and columns
 *   percent: the percentage of cells to retain
 *
 * output:
 *   thresh_mask: a boolean matrix filled with true for cells that are kept
 */
#include <cassert>
#include <cstdio>
#include <cstdlib>
#include <cstring>

#include <algorithm>

using namespace std;

extern int is_bench;
extern unsigned char randmat_matrix[20000][20000];
unsigned char thresh_mask[20000][20000];
static int histogram[100];

void thresh(int nrows, int ncols, int percent) {
  int nmax = 0;
  for (int i = 0; i < nrows; i++) {
    for (int j = 0; j < ncols; j++) {
      if (is_bench) {
        randmat_matrix[i][j] = (i * j) % 100;
      }
      nmax = max(nmax, (int)randmat_matrix[i][j]);
    }
  }

  for (int i = 0; i < nrows; i++) {
    for (int j = 0; j < ncols; j++) {
      histogram[randmat_matrix[i][j]]++;
    }
  }

  int count = (nrows * ncols * percent) / 100;

  int prefixsum = 0;
  int threshold = nmax;

  for (int i = nmax; i >= 0 && prefixsum <= count; i--) {
    prefixsum += histogram[i];
    threshold = i;
  }

  for (int i = 0; i < nrows; i++) {
    for (int j = 0; j < ncols; j++) {
      thresh_mask[i][j] = randmat_matrix[i][j] >= threshold;
    }
  }
}
