//go:build slow

package ftncode_test

import (
	"testing"

	ftncode "mochi/compiler/x/fortran"
)

func TestFortranCompiler(t *testing.T) {
	t.Skip("skipping heavy compiler test")
	_ = ftncode.New()
}
