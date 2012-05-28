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

use Search;

proc thresh(nrows: int, ncols: int,
    matrix: [1..nrows, 1..ncols] int, percent: int,
    mask: [1..nrows, 1..ncols] int) {
  var nmax = max reduce matrix;

  var histogram: [0..nmax] int;

  forall m in matrix {
    histogram[m] += 1;
  }

  var count: int = (nrows * ncols * percent) / 100;
  count = nrows * ncols - count;  // because scan starts from the beginning

  var prefixsum = + scan histogram;
  var threshold: int;
  var ind: int;
  var found: bool;

  (found, ind) = BinarySearch(prefixsum, count);
  threshold = ind - 1;

  forall i in matrix.domain {
    mask[i] = matrix[i] >= threshold;
  }
}

proc main() {
  var nrows: int;
  var ncols: int;
  var percent: int;

  read(nrows, ncols);

  var matrix: [1..nrows, 1..ncols] int;
  var mask: [1..nrows, 1..ncols] int;

  for i in 1..nrows do {
    for j in 1..ncols do {
      read(matrix[i,j]);
    }
  }

  read(percent);

  thresh(nrows, ncols, matrix, percent, mask);

  /*
  writeln(nrows, " ", ncols);

  for i in 1..nrows do {
    for j in 1..ncols do {
      write(mask[i, j], " ");
    }
    writeln();
  }
  writeln();//*/
}
