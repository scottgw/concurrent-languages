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

config const is_bench = false;

proc thresh(nrows: int, ncols: int,
    matrix: [1..nrows, 1..ncols] int, percent: int,
    mask: [1..nrows, 1..ncols] int) {
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

  if (!is_bench) {
    writeln(nrows, " ", ncols);

    for i in 1..nrows do {
      for j in 1..ncols do {
        write(mask[i, j], " ");
      }
      writeln();
    }
    writeln();
  }
}
