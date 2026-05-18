//go:build numregs_probe

// Diagnostic probe: print NumRegs and JIT compile status for every
// function in the FP-heavy BG corpora. Captured in MEP-39 §6.12.
//
// Run with:
//
//	go test -tags numregs_probe -run TestJITCoverageProbe -v ./runtime/jit/vm2jit/

package vm2jit

import (
	"fmt"
	"testing"

	"mochi/compiler2/corpus"
	"mochi/compiler2/emit"
	"mochi/compiler2/ir"
)

func TestJITCoverageProbe(t *testing.T) {
	cases := []struct {
		name string
		mod  *ir.Module
	}{
		{"mandelbrot", corpus.BuildMandelbrotKernel(200, 200, 50)},
		{"spectral_norm", corpus.BuildSpectralNorm(100)},
		{"n_body", corpus.BuildNBodyKernel()},
	}
	for _, c := range cases {
		prog, err := emit.Compile(c.mod)
		if err != nil {
			fmt.Printf("%s: emit err: %v\n", c.name, err)
			continue
		}
		for i, fn := range prog.Funcs {
			_, jitErr := Compile(fn)
			tag := "JIT-OK"
			if jitErr != nil {
				tag = fmt.Sprintf("JIT-skip(%v)", jitErr)
			}
			fmt.Printf("%-13s fn[%d] %-22s NumRegs=%2d  %s\n",
				c.name, i, fn.Name, fn.NumRegs, tag)
		}
		fmt.Println()
	}
}
