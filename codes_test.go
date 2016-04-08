package aep

import "testing"

func TestFIPS(t *testing.T) {
	r := new(SourceData)
	r.parseFIPS("\" 1010\"")
	if r.FIPS != "01010" {
		t.Errorf("want 01010; got %s", r.FIPS)
	}
}
