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

module Outer {

use Config;

inline
proc sqr(x: real): real {
  return x ** 2;
}

inline
proc distance(l, r: (int, int)): real {
  var lx, ly, rx, ry: real;
  (lx, ly) = l;
  (rx, ry) = r;
  return sqrt(sqr(lx - rx) + sqr(ly - ry));
}

proc outer(points: [pointSpace] (int, int), nelts: int):
    ([distSpace] real, [vectorSpace] real)
{
  var matrix: [distSpace] real;
  var vector: [vectorSpace] real;

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
 
 return (matrix, vector);
}
}
