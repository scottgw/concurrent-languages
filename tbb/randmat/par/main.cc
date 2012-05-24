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

static char matrix[10000][100000];

class RandMatCol {
public: 
  void operator() ( const blocked_range<size_t>& r ) const { 
    unsigned int seed = 0;
    for ( size_t j = r.begin(); j != r.end(); ++j ) {
      matrix[_row][j] = rand_r(&seed);
    }
  }
  RandMatCol(int row): _row(row) {}

private:
  int _row;
};


class RandMatRow {
public: 
  void operator() ( const blocked_range<size_t>& r ) const { 
    for ( size_t i = r.begin(); i != r.end(); ++i ) {
      parallel_for(blocked_range<size_t>(0, _ncols),
          RandMatCol(i), auto_partitioner());
    }
  }
  RandMatRow(int ncols): _ncols(ncols) {}

private:
  int _ncols;
};

void randmat(int nrows, int ncols, int s) {
  srand(s);
  parallel_for(blocked_range<size_t>(0, nrows),
      RandMatRow(ncols), auto_partitioner());
}

int main(int argc, char** argv) {
  int nrows, ncols, s;

  scanf("%d%d%d", &nrows, &ncols, &s);

  randmat(nrows, ncols, s);

  /*
  printf("%d %d\n", nrows, ncols);
  for (int i = 0; i < nrows; i++) {
    for (int j = 0; j < ncols; j++) {
      printf("%d ", matrix[i][j]);
    }
    printf("\n");
  }
  printf("\n");//*/

  return 0;
}
