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

#include "tbb/parallel_for.h"
#include "tbb/blocked_range.h"

using namespace tbb;

class RandMatCol {
public: 
  void operator() ( const blocked_range<size_t>& r ) const { 
    for ( size_t j = r.begin(); j != r.end(); ++j ) {
      _matrix[_row][j] = rand();
    }
  }
  RandMatCol(int row, int** matrix): _row(row), _matrix(matrix) {}

private:
  int _row;
  int** _matrix;
};


class RandMatRow {
public: 
  void operator() ( const blocked_range<size_t>& r ) const { 
    for ( size_t i = r.begin(); i != r.end(); ++i ) {
      parallel_for(blocked_range<size_t>(0, _ncols),
          RandMatCol(i, _matrix), auto_partitioner());
    }
  }
  RandMatRow(int ncols, int** matrix): _ncols(ncols), _matrix(matrix) {}

private:
  int _ncols;
  int** _matrix;
};

void randmat(int nrows, int ncols, int s, int** matrix) {
  srand(s);
  parallel_for(blocked_range<size_t>(0, nrows),
      RandMatRow(ncols, matrix), auto_partitioner());
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
