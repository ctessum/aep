/*
Copyright (C) 2012-2014 Regents of the University of Minnesota.
This file is part of AEP.

AEP is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

AEP is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with AEP.  If not, see <http://www.gnu.org/licenses/>.
*/

package main

import (
	"os"
	"path/filepath"
	"testing"
)

const (
	Tolerance = 1.e-5
)

var gopath string

func init() {
	gopath = os.Getenv("GOPATH")
}

func TestModelRun(t *testing.T) {
	config := filepath.Join(gopath, "src", "github.com", "ctessum", "aep", "aep",
		"scripts", "config2005nei.json")
	os.Args = append(os.Args, "-config="+config, "-testmode=true", "-seq") //, "-sectors=othon")
	main()
}
