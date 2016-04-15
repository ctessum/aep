package aep

import "testing"

func TestFIPS(t *testing.T) {
	r := new(SourceData)
	r.parseFIPS("\" 1010\"")
	if r.FIPS != "01010" {
		t.Errorf("want 01010; got %s", r.FIPS)
	}
}

func TestSCC(t *testing.T) {
	scc := ParseSCC("10800102")
	want := SCC{Tier1: "1", Tier2: "08", Tier3: "001", Tier4: "02"}
	if scc != want {
		t.Errorf("want %v, got %v", want, scc)
	}
	scc = ParseSCC("2210800102")
	want = SCC{Tier1: "22", Tier2: "10", Tier3: "800", Tier4: "102"}
	if scc != want {
		t.Errorf("want %v, got %v", want, scc)
	}
}
