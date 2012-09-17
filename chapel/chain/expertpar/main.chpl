/* chain: chain all problems
 * 
 * input:
 *   nelts: the number of elements
 *   randmat_seed: random number generator seed
 *   thresh_percent: percentage of cells to retain
 *   winnow_nelts: the number of points to select
 *
 * output:
 *   result: a real vector, whose values are the result of the final product
 */

use Config, Randmat, Thresh, Winnow, Outer, Product;

config const nelts = read (int),
             randmat_seed = read (int),
             thresh_percent = read (int),
             winnow_nelts = read (int);

proc main() {
  var matrix: [randSpace] int;
  var mask: [randSpace] bool;
  var points: [pointSpace] (int, int);
  var dists: [distSpace] real;
  var vec, result: [vectorSpace] real;

  writeln (nelts, randmat_seed);

  matrix = randmat(nelts, nelts, randmat_seed);
  writeln ("randmat");
  mask = thresh(matrix, nelts, nelts, thresh_percent);
  writeln ("thresh");
  points = winnow(matrix, mask, nelts, nelts, winnow_nelts);
  writeln ("winnow");
  (dists, vec) = outer(points, winnow_nelts);
  writeln ("outer");
  result = product(dists, vec, winnow_nelts);
  writeln ("product");

  if (!is_bench) {
    writeln(winnow_nelts);
    for i in 1..winnow_nelts do {
      write(result[i], " ");
    }
    writeln();
  }
}
