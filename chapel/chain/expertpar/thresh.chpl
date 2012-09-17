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
use Config;

var histogram: [histSpace] atomic int;

proc thresh(matrix: [randSpace] int,
            nrows: int, ncols: int, percent: int): [randSpace] bool{
  var nmax = max reduce matrix;

  forall (i,j) in randSpace do {
    histogram[matrix[i, j]].fetchAdd(1);
  }

  var count: int = (nrows * ncols * percent) / 100;

  var prefixsum: int = 0;
  var threshold: int = nmax;

  for i in histSpace do {
    if (prefixsum > count) then break;
    prefixsum += histogram[99 - i].read();
    threshold = 99 - i;
  }

  return matrix >= threshold;
}

}
