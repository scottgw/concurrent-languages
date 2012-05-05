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

proc swap(x: int, y: int, value: [?Dom]) {
  var v = value[x];
  value[x] = value[y];
  value[y] = v;
}

proc sort_impl(start: int, end: int, value: [?Dom]) {
  if (start >= end) {
    return;
  }
  var pivot_index = (start + end) / 2;
  var pivot = value[pivot_index];
  swap(pivot_index, end, value);
  var spot = start;
  for i in start..end do {
    if (value[i] < pivot) {
      swap(i, spot, value);
      spot += 1;
    }
  }
  swap(spot, end, value);
  pivot_index = spot;

  cobegin {
    sort_impl(start, pivot_index, value);
    sort_impl(pivot_index + 1, end, value);
  }
}

proc sort(n: int, value: [1..n] (int, (int, int))) {
  sort_impl(1, n, value);
}

proc winnow(nrows: int, ncols: int,
    matrix: [1..nrows, 1..ncols] int,
    mask: [1..nrows, 1..ncols] int,
    nelts: int,
    points: [1..nelts] (int, int)
    ) {

  var n: int = 0;
  
  n = + reduce mask;
  var values: [1..n] (int, (int, int));  // (value, (i, j))
  var can_go: sync bool = true;
  var count: int = 1;
  forall i in matrix.domain {
    if (mask[i] == 1) {
      if (can_go) {
        values[count] = (matrix[i], i);
        count += 1;
        can_go = true;
      }
    }
  }

  sort(n, values);

  var chunk: int = n / nelts;

  forall i in 1..nelts do {
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
