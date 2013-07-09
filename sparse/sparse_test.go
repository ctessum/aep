package sparse

import (
	"math"
	"testing"
)

const (
	Tolerance = 1.e-10
)

// diff determines if fractional difference between 2 numbers.
// is greater than the Tolerance
func diff(val1, val2 float64) bool {
	if val1 == 0. && val2 == 0. {
		return false
	} else {
		return math.Abs((val1-val2)/(val1+val2)*2) > Tolerance
	}
}

func TestSparse(t *testing.T) {
	a := ZerosSparse(5, 10, 15, 20)
	a.Set(10., 0, 0, 0, 0)
	a.Set(20., 2, 0, 0, 0)
	a.AddVal(30., 2, 3, 0, 0)
	a.AddVal(40., 2, 3, 0, 0)
	a.SubtractVal(40., 2, 3, 1, 0)
	a.SubtractVal(30., 2, 3, 1, 0)
	a.Set(40., 2, 3, 12, 0)
	a.Set(50., 2, 3, 12, 7)
	t.Log(a.Get(0, 0, 0, 0), 10)
	if diff(10, a.Get(0, 0, 0, 0)) {
		t.Fail()
	}
	t.Log(a.Get(2, 0, 0, 0), 20)
	if diff(20, a.Get(2, 0, 0, 0)) {
		t.Fail()
	}
	t.Log(a.Get(2, 3, 0, 0), 70)
	if diff(70, a.Get(2, 3, 0, 0)) {
		t.Fail()
	}
	t.Log(a.Get(2, 3, 1, 0), -70)
	if diff(-70, a.Get(2, 3, 1, 0)) {
		t.Fail()
	}
	t.Log(a.Get(2, 3, 12, 0), 40)
	if diff(40, a.Get(2, 3, 12, 0)) {
		t.Fail()
	}
	t.Log(a.Get(2, 3, 12, 7), 50)
	if diff(50, a.Get(2, 3, 12, 7)) {
		t.Fail()
	}
	a.Scale(10.)
	t.Log(a.Sum(), 1200)
	b := a.ScaleCopy(10.).Sum()
	t.Log(b, 12000)
	if diff(b, 12000.) {
		t.Fail()
	}
	c := a.ToDense()
	csum := 0.
	for _, val := range c {
		csum += val
	}
	t.Log(csum, 1200)
	if diff(csum, 1200.) {
		t.Fail()
	}
	index1d := a.Index1d([]int{2, 3, 12, 7})
	indexNd := a.IndexNd(index1d)
	t.Log(2, 3, 12, 7, indexNd)
	if indexNd[0] != 2 || indexNd[1] != 3 || indexNd[2] != 12 ||
		indexNd[3] != 7 {
		t.Fail()
	}
	index1d = a.Index1d([]int{2, 3, 0, 0})
	indexNd = a.IndexNd(index1d)
	t.Log(2, 3, 0, 0, indexNd)
	if indexNd[0] != 2 || indexNd[1] != 3 || indexNd[2] != 0 ||
		indexNd[3] != 0 {
		t.Fail()
	}
}

func TestMultiply(t *testing.T) {
	a := ZerosSparse(2, 2)
	b := ZerosSparse(2, 2)
	a.Set(1, 0, 0)
	b.Set(2, 0, 0)
	c := ArrayMultiply(a, b)
	if c.Sum() != 2 {
		t.Log("Fail on first try")
		t.Fail()
	}
	c = ArrayMultiply(a, b)
	if c.Sum() != 2 {
		t.Log("Fail on second try")
		t.Fail()
	}
}

func TestAdd(t *testing.T) {
	a := ZerosSparse(2, 2)
	b := ZerosSparse(2, 2)
	a.Set(1, 0, 0)
	b.Set(2, 0, 0)
	a.AddSparse(b)
	if a.Sum() != 3 {
		t.Log("Fail on first try")
		t.Fail()
	}
	a.AddSparse(b)
	if a.Sum() != 5 {
		t.Log("Fail on second try")
		t.Fail()
	}
}
