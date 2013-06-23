// A sparse array package based on
// https://github.com/skelterjohn/go.matrix/

package sparse

import (
	"fmt"
)

var BoundsCheck = true // Whether to check array bounds every time

// Sparse array with an arbitrary number of dimensions
type SparseArray struct {
	elements map[int]float64
	ndims    int
	dims     []int
	arrsize  int // Maximum number of elements in array
}

// Initialize a new array
func ZerosSparse(dims ...int) *SparseArray {
	A := new(SparseArray)
	A.elements = make(map[int]float64)
	A.ndims = len(dims)
	A.dims = dims
	A.arrsize = 1
	for _, i := range A.dims {
		A.arrsize *= i
	}
	return A
}

// Make sure index is within array dimensions
func (A *SparseArray) checkIndex(index []int) error {
	if BoundsCheck {
		if len(index) != A.ndims {
			err := fmt.Errorf("Index number of dimensions (%v) does not match "+
				"array number of dimensions.", len(index), A.ndims)
			return err
		}
		for i, dim := range A.dims {
			if index[i] >= dim {
				err := fmt.Errorf(
					"Index %v of dimension %v is greater than dimension "+
						"size %v.", index[i], i, A.dims[i])
				return err
			}
		}
	}
	return nil
}

// Make sure arrays are the same size
func (A *SparseArray) checkArray(B *SparseArray) error {
	if BoundsCheck {
		if B.ndims != A.ndims {
			err := fmt.Errorf("Number of dimensions in array A (%v) does "+
				"not match number of dimensions in array B (%v).", A.ndims, B.ndims)
			return err
		}
		for i, dim := range A.dims {
			if B.dims[i] != dim {
				err := fmt.Errorf(
					"Dimension %v is different in arrays A (%v) and B (%v).",
					i, A.dims[i], B.dims[i])
				return err
			}
		}
	}
	return nil
}

// Convert n-dimensional index to one-dimensional index
func (A *SparseArray) Index1d(index []int) (index1d int) {
	if err := A.checkIndex(index); err != nil {
		panic(err)
	}
	for i := 0; i < len(index); i++ {
		mul := 1
		for j := i + 1; j < len(index); j++ {
			mul = mul * A.dims[j]
		}
		index1d = index1d + index[i]*mul
	}
	return index1d
}

// Convert a 1-dimensional index to an n-dimensional index
func (A *SparseArray) IndexNd(index1d int) (indexNd []int) {
	leftover := index1d
	indexNd = make([]int, A.ndims)
	for i := 0; i < A.ndims; i++ {
		stride := 1
		for j := i + 1; j < A.ndims; j++ {
			stride *= A.dims[j]
		}
		indexNd[i] = leftover / stride
		if leftover >= stride {
			leftover = leftover % (indexNd[i] * stride)
		} else {
			leftover = 0
		}
	}
	return
}

// Set index to val.
func (A *SparseArray) Set(val float64, index ...int) {
	if err := A.checkIndex(index); err != nil {
		panic(err)
	}
	index1d := A.Index1d(index)
	A.elements[index1d] = val
}

// Get array value at index
func (A *SparseArray) Get(index ...int) float64 {
	if err := A.checkIndex(index); err != nil {
		panic(err)
	}
	index1d := A.Index1d(index)
	val, ok := A.elements[index1d]
	if ok {
		return val
	} else {
		return 0.
	}
}

// Get array value at one-dimensional index
func (A *SparseArray) Get1d(index1d int) float64 {
	val, ok := A.elements[index1d]
	if ok {
		return val
	} else {
		return 0.
	}
}

// Add val at array index
func (A *SparseArray) AddVal(val float64, index ...int) {
	if err := A.checkIndex(index); err != nil {
		panic(err)
	}
	index1d := A.Index1d(index)
	_, ok := A.elements[index1d]
	if ok {
		A.elements[index1d] += val
	} else {
		A.elements[index1d] = val
	}
}

// Add array B to array A in place.
func (A *SparseArray) AddSparse(B *SparseArray) {
	if err := A.checkArray(B); err != nil {
		panic(err)
	}
	for i, val := range B.elements {
		_, ok := A.elements[i]
		if ok {
			A.elements[i] += val
		} else {
			A.elements[i] = val
		}
	}
}

// Subtract array B from array A in place.
func (A *SparseArray) SubtractSparse(B *SparseArray) {
	if err := A.checkArray(B); err != nil {
		panic(err)
	}
	for i, val := range B.elements {
		_, ok := A.elements[i]
		if ok {
			A.elements[i] -= val
		} else {
			A.elements[i] = -1 * val
		}
	}
}

// Subtract val at array index
func (A *SparseArray) SubtractVal(val float64, index ...int) {
	if err := A.checkIndex(index); err != nil {
		panic(err)
	}
	index1d := A.Index1d(index)
	_, ok := A.elements[index1d]
	if ok {
		A.elements[index1d] -= val
	} else {
		A.elements[index1d] = -1 * val
	}
}

// Scale Multiplies entire array by val
func (A *SparseArray) Scale(val float64) {
	for i, _ := range A.elements {
		A.elements[i] *= val
	}
}

// ScaleCopy returns a copy of the array  multiplied by val
func (A *SparseArray) ScaleCopy(val float64) *SparseArray {
	out := A
	for i, _ := range A.elements {
		out.elements[i] *= val
	}
	return out
}

// Sum calculates the array sum.
func (A *SparseArray) Sum() float64 {
	sum := 0.
	for _, e := range A.elements {
		sum += e
	}
	return sum
}

// Nonzero returns (one dimensional) indicies of nonzero array elements
func (A *SparseArray) Nonzero() []int {
	index := make([]int, len(A.elements))
	i := 0
	for j, _ := range A.elements {
		index[i] = j
		i++
	}
	return index
}

func (A *SparseArray) ToArray() []float64 {
	out := make([]float64, A.arrsize)
	for i, val := range A.elements {
		out[i] = val
	}
	return out
}
