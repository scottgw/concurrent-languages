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


var matrix: [1..20000, 1..20000]int;
var mask: [1..20000, 1..20000]bool;
var count_per_line: [1..20001]int;
var points: [1..20000] (int, int);
var values: [0..20000] (int, (int, int)); // (value, i, j))

proc winnow(nelts: int) {
  var n: int = 0;

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
