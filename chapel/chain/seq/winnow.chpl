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

proc winnow(nrows: int, ncols: int,
    matrix: [1..nrows, 1..ncols] int,
    mask: [1..nrows, 1..ncols] bool,
    nelts: int,
    points: [1..nelts] (int, int)
    ) {

  var n: int = 0;
  for m in mask do {
    if (m) {
      n += 1;
    }
  }

  var values: [1..n] (int, (int, int));  // (value, (i, j))
  var count: int = 1;
  for i in matrix.domain {
    if (mask[i]) {
      values[count] = (matrix[i], i);
      count += 1;
    }
  }

  QuickSort(values);

  var chunk: int = n / nelts;

  for i in 1..nelts do {
    var ind: int;
    ind = (i - 1) * chunk + 1;
    (, points[i]) = values[ind];
  }
}
}
