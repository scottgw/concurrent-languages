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

proc winnow(nrows: int, ncols: int,
    matrix: [1..nrows, 1..ncols] int,
    mask: [1..nrows, 1..ncols] int,
    nelts: int,
    points: [1..nelts] (int, int)
    ) {

  var n: int = 0;
  for i in 1..nrows do {
    for j in 1..ncols do {
      if (mask[i, j] == 1) {
        n = n + 1;
      }
    }
  }

  var values: [1..n] (int, (int, int));
  var count: int = 1;
  for i in 1..nrows do {
    for j in 1..ncols do {
      if (mask[i, j] == 1) {
        values[count] = (matrix[i, j], (i, j));
        count = count + 1;
      }
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

proc read_matrix(nrows, ncols: int,
    matrix: [1..nrows, 1..ncols] int) {
  for i in 1..nrows do {
    for j in 1..ncols do {
      read(matrix[i, j]);
    }
  }
}

proc main() {
  var nrows: int;
  var ncols: int;
  var nelts: int;

  read(nrows, ncols);

  var matrix, mask: [1..nrows, 1..ncols] int;

  read_matrix(nrows, ncols, matrix);
  read_matrix(nrows, ncols, mask);

  read(nelts);

  var points: [1..nelts] (int, int);

  winnow(nrows, ncols, matrix, mask, nelts, points);

  writeln(nelts);

  for i in 1..nelts do {
    writeln(points[i]);
  }

  writeln();
}