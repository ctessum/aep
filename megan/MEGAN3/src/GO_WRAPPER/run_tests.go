package main

import "fmt"

func print_test_result(test_pass bool, name string, EPSILON float64) {
	if test_pass {
		fmt.Printf("### " + name + ": Pass! ### Go and standalone versions produce the same result (epsilon=%v)\n", EPSILON)
	} else {
		fmt.Printf("### " + name + ": Fail! ### Go and standalone versions produce different results (epsilon=%v)\n", EPSILON)
	}
}

func main() {	
	EPSILON := 1e-6 // precision for floating point comparison of outputs
	
	mgn2mech_test_pass := test_mgn2mech(EPSILON)
	megsea_test_pass := test_megsea(EPSILON)
	megcan_test_pass := test_megcan(EPSILON)
	megvea_test_pass := test_megvea(EPSILON)
	
	print_test_result(mgn2mech_test_pass, "MGN2MECH", EPSILON)
	print_test_result(megsea_test_pass, "MEGSEA", EPSILON)
	print_test_result(megcan_test_pass, "MEGCAN", EPSILON)
	print_test_result(megvea_test_pass, "MEGVEA", EPSILON)
}