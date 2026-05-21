package gogen

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"mochi/compiler3/ir"
)

// TestEmitLibraryMinimal asserts the multi-file layout includes the
// .go source and go.mod, and that the .go source declares the right
// package name.
func TestEmitLibraryMinimal(t *testing.T) {
	fn := ir.FixtureDouble()
	fn.Name = "Double" // capitalised → Go-public

	files, err := EmitLibrary(&Library{
		ModulePath: "example.com/mathy",
		Funcs:      []*ir.Function{fn},
	})
	if err != nil {
		t.Fatalf("EmitLibrary: %v", err)
	}
	names := SortedFilenames(files)
	want := []string{"go.mod", "mathy.go"}
	if len(names) != len(want) || names[0] != want[0] || names[1] != want[1] {
		t.Fatalf("filenames = %v, want %v", names, want)
	}
	src := string(files["mathy.go"])
	if !strings.Contains(src, "package mathy\n") {
		t.Errorf("missing package clause:\n%s", src)
	}
	if !strings.Contains(src, "func Double(") {
		t.Errorf("missing exported function:\n%s", src)
	}
	mod := string(files["go.mod"])
	if !strings.Contains(mod, "module example.com/mathy") {
		t.Errorf("go.mod missing module directive:\n%s", mod)
	}
	if !strings.Contains(mod, "go 1.22") {
		t.Errorf("go.mod missing go directive:\n%s", mod)
	}
	// No runtime/mochi import in this fixture → no `require mochi`.
	if strings.Contains(mod, "require mochi") {
		t.Errorf("go.mod has spurious require:\n%s", mod)
	}
}

// TestEmitLibraryWithRuntime asserts that when the emitted source
// references runtime/mochi (e.g. via a query op), go.mod declares the
// require, and the RuntimeReplace knob threads through.
func TestEmitLibraryWithRuntime(t *testing.T) {
	// Construct a function that uses query.Filter via FixtureIsEven.
	helper := ir.FixtureIsEven()
	demo := &ir.Function{Name: "Evens", Result: ir.TypeList}
	entry := demo.AddBlock()
	src := demo.AddValue(ir.Value{Type: ir.TypeList, Op: ir.OpNewList})
	v1 := demo.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpConst, Const: 1})
	v2 := demo.AddValue(ir.Value{Type: ir.TypeI64, Op: ir.OpConst, Const: 2})
	p1 := demo.AddValue(ir.Value{Type: ir.TypeUnit, Op: ir.OpListPushI64, Args: []uint32{src, v1}})
	p2 := demo.AddValue(ir.Value{Type: ir.TypeUnit, Op: ir.OpListPushI64, Args: []uint32{src, v2}})
	predRef := demo.AddValue(ir.Value{Type: ir.TypeClosure, Op: ir.OpFnRef, Const: 1})
	filt := demo.AddValue(ir.Value{Type: ir.TypeList, Op: ir.OpQueryFilter, Args: []uint32{src, predRef}})
	blk := demo.Block(entry)
	blk.Values = []uint32{src, v1, v2, p1, p2, predRef, filt}
	blk.Term = ir.Terminator{Kind: ir.TermReturn, Value: filt}

	files, err := EmitLibrary(&Library{
		ModulePath:     "example.com/runtimey",
		Funcs:          []*ir.Function{demo, helper},
		RuntimeReplace: "/path/to/mochi",
	})
	if err != nil {
		t.Fatalf("EmitLibrary: %v", err)
	}
	mod := string(files["go.mod"])
	if !strings.Contains(mod, "require mochi v0.0.0") {
		t.Errorf("go.mod missing mochi require:\n%s", mod)
	}
	if !strings.Contains(mod, "replace mochi => /path/to/mochi") {
		t.Errorf("go.mod missing replace directive:\n%s", mod)
	}
}

// TestEmitLibraryConsumerBuilds is the Phase 8 gate: emit a library,
// drop it on disk alongside a separate consumer Go module that
// imports it, and run `go test` on the consumer. The replace
// directive points the produced module's runtime at the mochi repo
// root so `mochi/runtime/mochi/query` resolves.
func TestEmitLibraryConsumerBuilds(t *testing.T) {
	// Build the produced library: an exported `Double` function.
	fn := ir.FixtureDouble()
	fn.Name = "Double"

	files, err := EmitLibrary(&Library{
		ModulePath: "example.com/mathy",
		Funcs:      []*ir.Function{fn},
	})
	if err != nil {
		t.Fatalf("EmitLibrary: %v", err)
	}

	// Lay out the produced library on disk.
	root := t.TempDir()
	libDir := filepath.Join(root, "mathy")
	if err := os.MkdirAll(libDir, 0o755); err != nil {
		t.Fatal(err)
	}
	for name, data := range files {
		if err := os.WriteFile(filepath.Join(libDir, name), data, 0o644); err != nil {
			t.Fatal(err)
		}
	}

	// Lay out a consumer module that imports the library and asserts
	// Double(21) == 42.
	consumerDir := filepath.Join(root, "consumer")
	if err := os.MkdirAll(consumerDir, 0o755); err != nil {
		t.Fatal(err)
	}
	consumerGoMod := "module example.com/consumer\n\ngo 1.22\n\nrequire example.com/mathy v0.0.0\n\nreplace example.com/mathy => ../mathy\n"
	if err := os.WriteFile(filepath.Join(consumerDir, "go.mod"), []byte(consumerGoMod), 0o644); err != nil {
		t.Fatal(err)
	}
	consumerTest := `package consumer_test

import (
	"testing"

	mathy "example.com/mathy"
)

func TestDouble(t *testing.T) {
	if got := mathy.Double(21); got != 42 {
		t.Fatalf("Double(21) = %d, want 42", got)
	}
}
`
	if err := os.WriteFile(filepath.Join(consumerDir, "consumer_test.go"), []byte(consumerTest), 0o644); err != nil {
		t.Fatal(err)
	}

	cmd := exec.Command("go", "test", ".")
	cmd.Dir = consumerDir
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("consumer go test failed: %v\noutput:\n%s\nlib source:\n%s", err, out, files["mathy.go"])
	}
}
