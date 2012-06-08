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

use Randmat;

var mask: [1..20000, 1..20000]bool;
var histogram: [0..99]int;

proc thresh(nrows: int, ncols: int, percent: int) {
  var nmax: int = 0;

  for i in 1..nrows do {
    for j in 1..ncols do {
      nmax = max(nmax, matrix[i, j]);
    }
  }

  for i in 1..nrows do {
    for j in 1..ncols do {
      histogram[matrix[i, j]] += 1;
    }
  }

  var count: int = (nrows * ncols * percent) / 100;

  var prefixsum: int = 0;
  var threshold: int = nmax;

  for i in 0..nmax do {
    if (prefixsum > count) then break;
    prefixsum += histogram[nmax - i];
    threshold = nmax - i;
  }

  for i in 1..nrows do {
    for j in 1..ncols do {
      mask[i, j] = matrix[i, j] >= threshold;
    }
  }
}
}
