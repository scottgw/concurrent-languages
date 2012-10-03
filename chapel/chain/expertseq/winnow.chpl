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

var values: [0..20000] (int, (int, int));

proc winnow(nrows: int, ncols: int, nelts: int) {
  var count = 0;
  for i in 1..nrows {
    for j in 1..ncols {
      if (mask[i, j]) {
        values[count] = (matrix[i, j], (i, j));
        count += 1;
      }
    }
  }

  QuickSort(values[0..count]);

  var chunk: int = count / nelts;

  forall i in 1..nelts do {
    var ind: int;
    ind = (i - 1) * chunk + 1;
    (, points[i]) = values[ind];
  }
}
}
