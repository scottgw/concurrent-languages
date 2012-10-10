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

use Config;

proc winnow(nrows: int, ncols: int, nelts: int) {
  var n: int = 0;
  var count_per_line: [1..nelts+1]int;
  var values: [0..nrows*ncols] (int, (int, int)); // (value, i, j))

  for i in 1..nrows do {
    for j in 1..ncols do {
      if (mask[i, j]) {
        n += 1;
      }
    }
  }

  var count: int = 1;
  for i in 1..nrows do {
    for j in 1..ncols do {
      if (mask[i, j]) {
        values[count] = (matrix[i, j], (i, j));
        count += 1;
      }
    }
  }

  QuickSort(values[0..n]);
  var chunk: int = n / nelts;

  for i in 1..nelts do {
    var ind: int;
    ind = (i - 1) * chunk + 1;
    (, points[i]) = values[ind];
  } 
}
}
