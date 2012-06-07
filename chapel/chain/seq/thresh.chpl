/* thresh: histogram thresholding
 * 
 * input:
 *   matrix: the integer matrix to be thresholded
 *   nrows, ncols: the number of rows and columns
 *   percent: the percentage of cells to retain
 *
 * output:
 *   mask: a boolean matrix filled with true for the cells to be kept
 */

module Thresh {
use Search;

proc thresh(nrows: int, ncols: int,
    matrix: [1..nrows, 1..ncols] int, percent: int,
    mask: [1..nrows, 1..ncols] bool) {
  var nmax: int = 0;
  for m in matrix do {
    nmax = max(nmax, m);
  }

  var histogram: [0..nmax] int;

  for m in matrix {
    histogram[m] += 1;
  }

  var count: int = (nrows * ncols * percent) / 100;

  var prefixsum: int = 0;
  var threshold: int = nmax;

  for i in 0..nmax do {
    if (prefixsum > count) then break;
    prefixsum += histogram[nmax - i];
    threshold = nmax - i;
  }

  for i in matrix.domain {
    mask[i] = matrix[i] >= threshold;
  }
}

}
