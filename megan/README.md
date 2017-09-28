# Go interface to [MEGAN3 beta](https://sites.google.com/uci.edu/bai/megan/versions)

## Compilation

The Go source code is in the `megan` directory and the FORTRAN in the `fortran` subdirectory

### FORTRAN static library

The FORTRAN must first be compiled in a static library `libmegan.a`.

1. `cd fortran`
2. `make all`

### Go interface code

The Go interface code can then be compiled using `go build` from the `megan` directory.

## Testing

Tests for all 4 MEGAN components MEGSEA, MEGCAN, MEGVEA and MGN2MECH are available in 2 versions.

* A standard version that compares the output of the Go interface against hardcoded results from the standalone version
* A standalone version that compares the output of the Go interface directly against the standalone version.
  **Requirements** Installing the standalone MEGAN3 beta by running `./make_all_programs.scr 64bit` from the `megan/MEGAN3/src/` directory
  
The standard version of the tests can be run using `go test` from the `megan` directory.

Running the standalone version first requires setting the `MEGAN_STANDALONE_TEST` environment variable.