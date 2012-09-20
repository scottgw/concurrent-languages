/*
 * randmat: random number generation
 *
 * input:
 *   nrows, ncols: the number of rows and columns
 *   s: the seed
 *
 * output:
 *   martix: a nrows x ncols integer matrix
 *
 */
package main

import (
	"flag"
	"fmt"
)

type ByteMatrix struct {
	Rows, Cols int
	array      []byte
}

func NewByteMatrix(r, c int) *ByteMatrix {
	return &ByteMatrix{r, c, make([]byte, r*c)}
}

func (m *ByteMatrix) Row(i int) []byte {
	return m.array[i*m.Cols : (i+1)*m.Cols]
}

func WrapBytes(r, c int, bytes []byte) *ByteMatrix {
	return &ByteMatrix{r, c, bytes}
}

func (m *ByteMatrix) Bytes() []byte {
	return m.array[0 : m.Rows*m.Cols]
}
const (
	LCG_A = 1664525
	LCG_C = 1013904223
)

var (
	is_bench = flag.Bool("is_bench", false, "")
	nelts   = flag.Int("N", 10, "nelts")
	seed     = flag.Uint("S", 8, "seed")
  thresh_percent =  flag.Int ("P", 100, "threshold_percent")
	winnow_nelts    = flag.Int("WN", 9, "winnow_nelts")
)

func Randmat(nelts int, s uint32) *ByteMatrix {
	matrix := NewByteMatrix(nelts, nelts)

  for i := 0; i < nelts; i++ {
		var seed = s + uint32(i)
		row := matrix.Row(i)
		for j := range row {
			seed = LCG_A*seed + LCG_C
			row[j] = byte(seed%100) % 100
		}
	}
	return matrix
}

func Thresh(m *ByteMatrix, nelts, percent int) (mask[]bool) {
	var hist [100]int

	for _, v := range m.Bytes() {
		hist[v]++
	}

	count := (nelts * nelts * percent) / 100
	prefixsum := 0

	for	threshold := 99 ; threshold > 0; threshold-- {
		prefixsum += hist[threshold]
		if prefixsum > int(count) {
			break
		}
	}

  for i := 0; i < nelts; i++ {
		row := m.Row(i)
		for j := range row {
			mask[i*nelts + j] = row[j] >= byte(threshold)
		}
	}

  return
}


func main() {
	flag.Parse()

	rand_matrix := Randmat(*nelts, uint32(*seed))
  mask := Thresh (rand_matrix, *nelts, *thresh_percent)
  win_pts := Winnow (rand_matrix, mask, *nelts, *winnow_nelts)

  mask[0] = true
  var result []float64
	if !*is_bench {
		for _, v := range result {
      fmt.Printf("%g ", v)
    }
	}
}
