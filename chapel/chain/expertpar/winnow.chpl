/* winnow: weighted point selection
 * 
 * input:
 *   matrix: an integer matrix, whose values are used as masses
 *   mask: a boolean matrix showing which points are eligible for
 *     consideration
 *   nrows, ncols: the number of rows and columns
 *   nelts: the number of points to select
 *
 * output:
 *   points: a vector of (x, y) points
 */

module Winnow {
config const is_bench = false;

use Config;

proc winnow(matrix: [randSpace] int,
            mask: [randSpace] bool, 
            nrows: int, ncols: int, nelts: int): [pointSpace] (int, int) {
  var n: int = 0;
  var count_per_line: [1..20001] int;
  var points: [pointSpace] (int, int);
  var values: [0..20000] (int, (int, int)); // (value, i, j))


  forall i in 1..nrows do {
    count_per_line[i + 1] = 0;
    for j in 1..ncols do {
      if (is_bench) {
        mask[i, j] = (((i - 1) * (j - 1)) % (ncols + 1)) == 1;
      }
      count_per_line[i + 1] += mask[i, j];
    }
  }

  var total = + scan count_per_line;
  n = total[nrows + 1];

  forall i in 1..nrows do {
    var count = total[i];
    for j in 1..ncols do {
      if (mask[i, j]) {
        values[count] = (matrix[i, j], (i, j));
        count += 1;
      }
    }
  }

  QuickSort(values[0..n]);

  var chunk: int = n / nelts;

  forall i in 1..nelts do {
    var ind: int;
    ind = (i - 1) * chunk + 1;
    (, points[i]) = values[ind];
  }

  return points;
}
}
