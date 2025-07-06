//go:build ocamlroundtrip

package mlcode_test

import (
	"fmt"
	"path/filepath"
	"testing"

	mlcode "mochi/compile/x/ocaml"
	any2mochi "mochi/tools/any2mochi"
)

func TestOCaml_RoundTripVM(t *testing.T) {
	if err := mlcode.EnsureOCaml(); err != nil {
		t.Skipf("ocamlc not installed: %v", err)
	}
	root := any2mochi.FindRepoRoot(t)
	pattern := filepath.Join(root, "tests", "vm", "valid", "*.mochi")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", pattern)
	}
	var errs []string
	for _, src := range files {
		name := filepath.Base(src)
		t.Run(name, func(t *testing.T) {
			if err := mlcode.RoundTripRun(src); err != nil {
				errs = append(errs, fmt.Sprintf("%s: %v", name, err))
				t.Log(err)
			}
		})
	}
	any2mochi.WriteErrorsMarkdown(filepath.Join(root, "compile", "x", "ocaml"), errs)
}
