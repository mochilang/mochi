//go:build slow

package ftncode_test

import (
	"testing"

	ftncode "mochi/compiler/x/fortran"
)

// ensureFortran returns the gfortran executable path or skips the test if the
// compiler is not installed.
func ensureFortran(t *testing.T) string {
	t.Helper()
	path, err := ftncode.EnsureFortran()
	if err != nil {
		t.Skipf("gfortran missing: %v", err)
	}
	return path
}
