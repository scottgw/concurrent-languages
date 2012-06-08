/* outer: outer product
 * 
 * input:
 *   points: a vector of (x, y) points
 *   nelts: the number of points
 *
 * output:
 *   matrix: a real matrix, whose values are filled with inter-point
 *     distances
 *   vector: a real vector, whose values are filled with origin-to-point
 *     distances
 */

config const is_bench = false;

var matrix: [1..10000, 1..10000]real;
var vector: [1..10000]real;
var points: [1..10000](int, int);

proc sqr(x: real): real {
  return x * x;
}

proc distance(l, r: (int, int)): real {
  var lx, ly, rx, ry: real;
  (lx, ly) = l;
  (rx, ry) = r;
  return sqrt(sqr(lx - rx) + sqr(ly - ry));
}

proc outer(nelts: int) {
  forall i in 1..nelts do {
    var nmax: real = -1;
    for j in 1..nelts do {
      if (i != j) {
        matrix[i, j] = distance(points[i], points[j]);
        nmax = max(nmax, matrix[i, j]);
      }
    }
    matrix[i, i] = nmax * nelts;
    vector[i] = distance((0, 0), points[i]);
  }
}

proc read_vector_of_points(nelts: int) {
  var a, b: int;
  for i in 1..nelts do {
    read(a, b);
    points[i] = (a, b);
  }
}

proc main() {
  var nelts: int;
  read(nelts);

  var points: [1..nelts] (int, int);

  if (!is_bench) {
    read_vector_of_points(nelts);
  }

  outer(nelts);

  if (!is_bench) {
    writeln(nelts + " " + nelts);
    for i in 1..nelts do {
      for j in 1..nelts do {
        write(matrix[i, j] + " ");
      }
      writeln();
    }
    writeln();

    writeln(nelts);
    for i in 1..nelts do {
      write(vector[i] + " ");
    }
    writeln();
  }
}
