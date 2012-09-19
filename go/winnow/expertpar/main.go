/*
 * winnow: weighted point selection
 *
 * input:
 *   matrix: an integer matrix, whose values are used as masses
 *   mask: a boolean matrix showing which points are eligible for
 *     consideration
 *   nrows, ncols: the number of rows and columns
 *   nelts: the number of points to select
 *
 * output:
 *   points: a vector of (x, y) points
 *
 :b*/
package main

import (
	"flag"
	"fmt"
	"runtime"
	"sort"
)

type ByteMatrix struct {
	Rows, Cols int
	array      []byte
}

func WrapBytes(r, c int, bytes []byte) *ByteMatrix {
	return &ByteMatrix{r, c, bytes}
}

func NewByteMatrix(r, c int) *ByteMatrix {
	return &ByteMatrix{r, c, make([]byte, r*c)}
}

func (m *ByteMatrix) Row(i int) []byte {
	return m.array[i*m.Cols : (i+1)*m.Cols]
}

func (m *ByteMatrix) Bytes() []byte {
	return m.array[0 : m.Rows*m.Cols]
}

var is_bench = flag.Bool("is_bench", false, "")
var matrix []byte
var mask [20000][20000]bool
var points []int

type WinnowPoints struct {
	m *ByteMatrix
	e []int // indexes into the ByteMatrix 'm'
}

func (p *WinnowPoints) Len() int {
	return len(p.e)
}

func (p *WinnowPoints) Swap(i, j int) {
	p.e[i], p.e[j] = p.e[j], p.e[i]
}

func (p *WinnowPoints) Less(i, j int) bool {
	if p.m.array[p.e[i]] != p.m.array[p.e[j]] {
		return p.m.array[p.e[i]] < p.m.array[p.e[j]]
	}

	return p.e[i] < p.e[j]
}

func Winnow(m *ByteMatrix, nrows, ncols, nelts int) {
	NP := runtime.GOMAXPROCS(0)
	var values WinnowPoints
	values.m = m

	values_work := make(chan int)
	values_done := make(chan []int)

	go func() {
		for i := 0; i < nrows; i++ {
			values_work <- i
		}
		close(values_work)
	}()

	for i := 0; i < NP; i++ {
		go func() {
      var local_indexes []int
			for i := range values_work {
				for j := 0; j < ncols; j++ {
					if *is_bench {
						mask[i][j] = ((i * j) % (ncols + 1)) == 1
					}

					if mask[i][j] {
						idx := i*(nrows+1) + j
						local_indexes = append(local_indexes, idx)
					}
				}
			}
			values_done <- local_indexes
		}()
	}

  var accum []int
	for i := 0; i < NP; i++ {
		local_indexes := <-values_done
    temp_slice := make ([]int, len(accum) + len (local_indexes))
    copy (temp_slice, accum)
    copy (temp_slice [len(accum):], local_indexes)
    accum = temp_slice
	}

  values.e = accum

	sort.Sort(&values)

	chunk := values.Len() / nelts

	point_work := make(chan int)
	point_done := make(chan bool)
	go func() {
		for i := 0; i < nelts; i++ {
			point_work <- i
		}
		close(point_work)
	}()

	for i := 0; i < NP; i++ {
		go func() {
			for i := range point_work {
				points[i] = values.e[i*chunk]
			}
			point_done <- true
		}()
	}

	for i := 0; i < NP; i++ {
		<-point_done
	}
}

func read_integer() int {
	var value int
	for true {
		var read, _ = fmt.Scanf("%d", &value)
		if read == 1 {
			break
		}
	}
	return value
}

func read_matrix(nrows, ncols int) {
	for i := 0; i < nrows; i++ {
		for j := 0; j < ncols; j++ {
			matrix[i*(nrows+1)+j] = byte(read_integer())
		}
	}
}

func read_mask(nrows, ncols int) {
	for i := 0; i < nrows; i++ {
		for j := 0; j < ncols; j++ {
			mask[i][j] = (read_integer() == 1)
		}
	}
}

func main() {
	var nrows, ncols, nelts int

	flag.Parse()

	nrows = int(read_integer())
	ncols = int(read_integer())

	m := NewByteMatrix(nrows, ncols)
	points = make([]int, 10000)
	matrix = m.array

	if !*is_bench {
		read_matrix(nrows, ncols)
		read_mask(nrows, ncols)
	}

	nelts = int(read_integer())

	Winnow(m, nrows, ncols, nelts)

	if !*is_bench {
		fmt.Printf("%d\n", nelts)
		for i := 0; i < nelts; i++ {
			fmt.Printf("%d %d\n", points[i]/ncols, points[i]%ncols)
		}
		fmt.Printf("\n")
	}
}
