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
use Random;

proc randmat(nrows: int, ncols: int, s: int,
    matrix: [1..nrows, 1..ncols] int) {
  const RAND_MAX: int = 1000;

  var rand = new RandomStream(2 * s + 1); // s must be odd
  forall m in matrix {
    m = floor(rand.getNext() * 1000) : int;
  }
}

}
