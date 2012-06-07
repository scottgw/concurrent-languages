/*
 * randmat: random number generation
 *
 * input:
 *   nrows, ncols: the number of rows and columns
 *   s: the seed
 *
 * output:
 *   Randmat_matrix: a nrows x ncols integer matrix
 *
 */
package all

var Randmat_matrix [20000][20000]byte;

func Randmat(nrows, ncols, seed int) {
  LCG_A := 1664525;
  LCG_C := 1013904223;
  for i := 0; i < nrows; i++ {
    for j := 0; j < ncols; j++ {
      seed = LCG_A * seed + LCG_C;
      Randmat_matrix[i][j] = byte(seed % 100) % 100;
    }
  }
}
