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

proc main() {
  const INT_MAX: int = 2147483647;

  var nrows: int;
  var ncols: int;
  var s: int;

  read(nrows, ncols, s);
  s = 2 * s + 1; // s must be odd
  writeln(nrows, " ", ncols);

  var rand = new RandomStream(s);
  for i in 1..nrows do {
    for j in 1..ncols do {
      var cur: int;
      cur = floor(rand.getNext() * INT_MAX) : int;
      write(cur, " ");
    }
    writeln();
  }
  writeln();
}
