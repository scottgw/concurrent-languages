/* randmat: random number generation
 * 
 * input:
 *   nrows, ncols: the number of rows and columns
 *   s: the seed
 *
 * output:
 *   matrix: an nrows x ncols integer matrix
 */

module Randmat {
var matrix: [1..20000, 1..20000]int;

proc randmat(nrows: int, ncols: int, s: int) {
  const LCG_A: int = 1664525;
  const LCG_C: int = 1013904223;
  var seed = s;

  for i in 1..nrows do {
    for j in 1..ncols do {
      seed = LCG_A * seed + LCG_C;
      matrix[i, j] = abs(seed) % 100;
    }
  }
}
}
