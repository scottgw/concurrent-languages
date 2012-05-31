/* randmat: random number generation
 * 
 * input:
 *   nrows, ncols: the number of rows and columns
 *   s: the seed
 *
 * output:
 *   matrix: an nrows x ncols integer matrix
 */

use Random;

config const is_bench = false;

proc randmat(nrows: int, ncols: int, s: int,
    matrix: [1..nrows, 1..ncols] int) {
  const INT_MAX: int = 2147483647;

  var rand = new RandomStream(2 * s + 1, false); // s must be odd
  forall m in matrix {
    m = floor(rand.getNext() * INT_MAX) : int;
  }
}

proc main() {
  var nrows: int;
  var ncols: int;
  var s: int;

  read(nrows, ncols, s);

  var matrix: [1..nrows, 1..ncols] int;

  randmat(nrows, ncols, s, matrix);

  if (!is_bench) {
    writeln(nrows, " ", ncols);

    for i in 1..nrows do {
      for j in 1..ncols do {
        write(matrix[i, j], " ");
      }
      writeln();
    }
    writeln();
  }
}
