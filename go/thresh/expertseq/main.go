/*
 * thresh: histogram thresholding
 *
 * input:
 *   matrix: the integer matrix to be thresholded
 *   nrows, ncols: the number of rows and columns
 *   percent: the percentage of cells to retain
 *
 * output:
 *   mask: a boolean matrix filled with true for cells that are kept
 *
 */
package main

import (
	"flag"
	"fmt"
)

var is_bench = flag.Bool("is_bench", false, "")

var matrix [20000][20000]byte
var mask [20000][20000]bool
var histogram [100]int

func max(a, b int) int {
	if a > b {
		return a
	}
	return b
}

func thresh(nrows, ncols int, percent int) {
	for i := 0; i < nrows; i++ {
		for j := 0; j < ncols; j++ {
			if *is_bench {
				matrix[i][j] = byte((i * j) % 100)
			}
			histogram[matrix[i][j]]++
		}
	}

	count := (nrows * ncols * percent) / 100
	prefixsum := 0
	threshold := 99

	for ; threshold > 0; threshold-- {
		prefixsum += histogram[threshold]
		if prefixsum > count {
			break
		}
	}
	for i := 0; i < nrows; i++ {
		for j := 0; j < ncols; j++ {
			mask[i][j] = matrix[i][j] >= byte(threshold)
		}
	}
}

func main() {
	var nrows, ncols, percent int

	flag.Parse()

	fmt.Scanf("%d%d", &nrows, &ncols)

	if !*is_bench {
		for i := 0; i < nrows; i++ {
			for j := 0; j < ncols; j++ {
				fmt.Scanf("%d", &matrix[i][j])
			}
		}
	}

	fmt.Scanf("%d", &percent)

	thresh(nrows, ncols, percent)

	if !*is_bench {
		for i := 0; i < nrows; i++ {
			for j := 0; j < ncols; j++ {
				if mask[i][j] {
					fmt.Printf("1 ")
				} else {
					fmt.Printf("0 ")
				}
			}
			fmt.Printf("\n")
		}
		fmt.Printf("\n")
	}
}
