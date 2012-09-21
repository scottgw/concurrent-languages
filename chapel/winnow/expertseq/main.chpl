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

config const is_bench = false;
config const nrows = read(int),
             ncols = read(int);

const MatrixSpace = [1..20000, 1..20000];
const SubMatrixSpace = [1..nrows, 1..ncols];

var matrix: [MatrixSpace] int;
var mask: [MatrixSpace] bool;
var points: [1..20000] (int, int);
var values: [0..20000] (int, (int, int)); // (value, i, j))

proc winnow(nelts: int) {
  var count: int = 0;

  for (i,j) in SubMatrixSpace do {
    if (is_bench) {
      mask[i, j] = (((i - 1) * (j - 1)) % (ncols + 1)) == 1;
    }

    if (mask[i, j]) {
      values[count] = (matrix[i, j], (i, j));
      count += 1;
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

proc read_matrix() {
  for i in 1..nrows do {
    for j in 1..ncols do {
      read(matrix[i, j]);
    }
  }
}

proc read_mask() {
  for i in 1..nrows do {
    for j in 1..ncols do {
      var v: int;
      read(v);
      mask[i, j] = v == 1;
    }
  }
}

proc main() {
  var nelts: int;

  if (!is_bench) {
    read_matrix();
    read_mask();
  }

  read(nelts);

  winnow(nelts);

  if (!is_bench) {
    writeln(nelts);

    for i in 1..nelts do {
      writeln(points[i]);
    }

    writeln();
  }
}
