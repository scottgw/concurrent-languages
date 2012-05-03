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
use Randmat, Thresh, Winnow, Outer, Product;

proc main() {
  var nelts, randmat_seed, thresh_percent, winnow_nelts: int;
  read(nelts, randmat_seed, thresh_percent, winnow_nelts);

  var randmat_matrix, thresh_mask: [1..nelts, 1..nelts] int;
  var winnow_points: [1..winnow_nelts] (int, int);
  var outer_matrix: [1..winnow_nelts, 1..winnow_nelts] real;
  var outer_vector, product_result: [1..winnow_nelts] real;

  randmat(nelts, nelts, randmat_seed, randmat_matrix);
  thresh(nelts, nelts, randmat_matrix, thresh_percent, thresh_mask);
  winnow(nelts, nelts, randmat_matrix, thresh_mask, winnow_nelts,
      winnow_points);
  outer(winnow_nelts, winnow_points, outer_matrix, outer_vector);
  product(winnow_nelts, outer_matrix, outer_vector, product_result);

  writeln(winnow_nelts);
  for i in 1..winnow_nelts do {
    write(product_result[i], " ");
  }
  writeln();
}
