package monomorphise

import (
	"crypto/sha256"
	"encoding/hex"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"mochi/package3/go/apisurface"
)

// TestPhase15MonomorphiseSentinel renders a monomorphised wrapper for a
// tiny synthetic generic function and asserts that `go build ./...`
// against the rendered source compiles cleanly. The synthetic source
// module is dropped into the same scratch tree so the wrapper resolves
// the generic via a relative replace directive.
func TestPhase15MonomorphiseSentinel(t *testing.T) {
	dir := t.TempDir()

	// Source module: exports two generic funcs.
	srcDir := filepath.Join(dir, "src")
	if err := os.MkdirAll(srcDir, 0o755); err != nil {
		t.Fatal(err)
	}
	writeFile(t, filepath.Join(srcDir, "go.mod"), "module example.com/src\n\ngo 1.21\n")
	writeFile(t, filepath.Join(srcDir, "src.go"), `package src

func Sort[T any](xs []T) []T { return xs }

func Pair[K any, V any](k K, v V) (K, V) { return k, v }
`)

	// Wrapper module: imports the source as src_aliased and emits two
	// monomorphised wrappers.
	wrapDir := filepath.Join(dir, "wrap")
	if err := os.MkdirAll(wrapDir, 0o755); err != nil {
		t.Fatal(err)
	}
	writeFile(t, filepath.Join(wrapDir, "go.mod"),
		"module example.com/wrap\n\ngo 1.21\n\nrequire example.com/src v0.0.0\n\nreplace example.com/src => ../src\n")

	pkg := apisurface.Package{
		ImportPath: "example.com/src",
		Funcs: []apisurface.Func{
			{
				Name:       "Sort",
				TypeParams: []apisurface.TypeParam{{Name: "T", Constraint: "any"}},
				Params:     []apisurface.Param{{Name: "xs", Type: "[]T"}},
				Results:    []apisurface.Param{{Type: "[]T"}},
			},
			{
				Name:       "Pair",
				TypeParams: []apisurface.TypeParam{{Name: "K"}, {Name: "V"}},
				Params:     []apisurface.Param{{Name: "k", Type: "K"}, {Name: "v", Type: "V"}},
				Results:    []apisurface.Param{{Type: "K"}, {Type: "V"}},
			},
		},
	}
	set := &SpecSet{Specs: []Spec{
		{Item: "example.com/src.Sort", TypeArgs: []string{"int64"}},
		{Item: "example.com/src.Pair", TypeArgs: []string{"string", "int64"}},
	}}
	instances, missing, err := Resolve(pkg, set)
	if err != nil {
		t.Fatalf("Resolve: %v", err)
	}
	if len(missing) != 0 {
		t.Fatalf("unexpected missing: %v", missing)
	}
	if len(instances) != 2 {
		t.Fatalf("instances = %d", len(instances))
	}

	header := "package wrap\n\nimport src_aliased \"example.com/src\"\n\n// Force-use the import for the void wrapper case.\nvar _ = src_aliased.Sort[int64]\n\n"
	var sb strings.Builder
	sb.WriteString(header)
	for _, inst := range instances {
		// Strip the //export directive so the test compiles without cgo.
		out, err := RenderInstance(inst, "src_v0_0_0", "src_aliased")
		if err != nil {
			t.Fatalf("RenderInstance(%q): %v", inst.Spec.Item, err)
		}
		out = stripExport(out)
		sb.WriteString(out)
		sb.WriteString("\n")
	}
	writeFile(t, filepath.Join(wrapDir, "wrap.go"), sb.String())

	cmd := exec.Command("go", "build", "./...")
	cmd.Dir = wrapDir
	cmd.Env = append(os.Environ(), "GO111MODULE=on")
	if out, err := cmd.CombinedOutput(); err != nil {
		t.Fatalf("go build failed: %v\noutput:\n%s\nwrap.go was:\n%s", err, out, sb.String())
	}
}

// TestPhase15RenderInstanceDeterministic locks in the byte-determinism
// of RenderInstance: the same input must yield the same SHA-256 across
// 10 calls. This is load-bearing for the phase 10 wrapper-sha256 pin.
func TestPhase15RenderInstanceDeterministic(t *testing.T) {
	inst := Instance{
		Spec: Spec{Item: "x.F", TypeArgs: []string{"int64", "string"}},
		Func: apisurface.Func{
			Name:       "F",
			TypeParams: []apisurface.TypeParam{{Name: "K"}, {Name: "V"}},
			Params:     []apisurface.Param{{Name: "k", Type: "K"}, {Name: "v", Type: "V"}},
			Results:    []apisurface.Param{{Type: "K"}, {Type: "V"}},
		},
		SymbolSuffix: "int64_string",
	}
	var first string
	for i := 0; i < 10; i++ {
		out, err := RenderInstance(inst, "m", "src")
		if err != nil {
			t.Fatalf("RenderInstance: %v", err)
		}
		sum := sha256.Sum256([]byte(out))
		hex := hex.EncodeToString(sum[:])
		if i == 0 {
			first = hex
			continue
		}
		if hex != first {
			t.Fatalf("non-deterministic render on iteration %d: %s vs %s", i, hex, first)
		}
	}
}

func stripExport(s string) string {
	var keep []string
	for _, line := range strings.Split(s, "\n") {
		if strings.HasPrefix(strings.TrimSpace(line), "//export ") {
			continue
		}
		keep = append(keep, line)
	}
	return strings.Join(keep, "\n")
}

func writeFile(t *testing.T, path, content string) {
	t.Helper()
	if err := os.WriteFile(path, []byte(content), 0o644); err != nil {
		t.Fatalf("write %s: %v", path, err)
	}
}
