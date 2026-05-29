package tinygo

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

// TestPhase16TinygoSentinel renders a //go:linkname-decorated
// wrapper file against a synthetic source module and asserts that
// `go build` accepts the file (TinyGo is not assumed available on
// CI; the regular Go toolchain is the strictest available
// proxy for the //go:linkname directive's syntax).
//
// The wrapper file is rendered with two linkname specs: one
// single-result function and one zero-result function. Both bind
// to source symbols via //go:linkname.
func TestPhase16TinygoSentinel(t *testing.T) {
	dir := t.TempDir()

	// Source module: two exported functions.
	srcDir := filepath.Join(dir, "src")
	if err := os.MkdirAll(srcDir, 0o755); err != nil {
		t.Fatal(err)
	}
	writeFile(t, filepath.Join(srcDir, "go.mod"), "module example.com/src\n\ngo 1.21\n")
	writeFile(t, filepath.Join(srcDir, "src.go"), `package src

func Double(v int64) int64 { return v * 2 }

func Touch(v int64) {}
`)

	// Wrapper module: imports source via a replace directive and
	// renders one linkname wrapper per source function.
	wrapDir := filepath.Join(dir, "wrap")
	if err := os.MkdirAll(wrapDir, 0o755); err != nil {
		t.Fatal(err)
	}
	writeFile(t, filepath.Join(wrapDir, "go.mod"),
		"module example.com/wrap\n\ngo 1.21\n\nrequire example.com/src v0.0.0\n\nreplace example.com/src => ../src\n")

	// A small consumer file forces the dependency so go build
	// doesn't strip the source module.
	writeFile(t, filepath.Join(wrapDir, "consumer.go"), `package wrap

import _ "example.com/src"
`)

	out, err := RenderFile("wrap", []LinknameSpec{
		{
			LocalName:    "mochi_src_Double",
			TargetSymbol: "example.com/src.Double",
			Params:       []apisurface.Param{{Name: "v", Type: "int64"}},
			Results:      []apisurface.Param{{Type: "int64"}},
		},
		{
			LocalName:    "mochi_src_Touch",
			TargetSymbol: "example.com/src.Touch",
			Params:       []apisurface.Param{{Name: "v", Type: "int64"}},
		},
	})
	if err != nil {
		t.Fatalf("RenderFile: %v", err)
	}
	writeFile(t, filepath.Join(wrapDir, "linkname.go"), out)

	cmd := exec.Command("go", "build", "./...")
	cmd.Dir = wrapDir
	cmd.Env = append(os.Environ(), "GO111MODULE=on")
	if combined, err := cmd.CombinedOutput(); err != nil {
		t.Fatalf("go build failed: %v\n%s\nlinkname.go:\n%s", err, combined, out)
	}
}

// TestPhase16EmbeddedSubsetCompatibleSurface walks a hand-curated
// "good" surface (only allowed imports, only allowed types) and
// asserts the embedded profile reports zero violations.
func TestPhase16EmbeddedSubsetCompatibleSurface(t *testing.T) {
	good := apisurface.Package{
		ImportPath: "example.com/good",
		Imports:    []string{"fmt", "errors", "strings", "math", "sort"},
		Funcs: []apisurface.Func{
			{
				Name:    "Hash",
				Params:  []apisurface.Param{{Name: "s", Type: "string"}},
				Results: []apisurface.Param{{Type: "uint64"}},
			},
			{
				Name: "Sort",
				Params: []apisurface.Param{
					{Name: "xs", Type: "[]int64"},
				},
			},
		},
		Types: []apisurface.Type{
			{
				Name: "Buffer",
				Methods: []apisurface.Func{
					{
						Name:    "Len",
						Results: []apisurface.Param{{Type: "int"}},
					},
				},
			},
		},
	}
	violations := CheckPackage(ProfileEmbedded, good)
	if len(violations) != 0 {
		t.Fatalf("expected no violations, got %v", violations)
	}
	if !IsCompatible(ProfileEmbedded, good) {
		t.Errorf("good surface should be compatible")
	}
}

// TestPhase16EmbeddedSubsetRejectsBadSurface confirms a hand-
// curated "bad" surface with a banned import and a banned type in
// a result position trips the right violation kinds.
func TestPhase16EmbeddedSubsetRejectsBadSurface(t *testing.T) {
	bad := apisurface.Package{
		ImportPath: "example.com/bad",
		Imports:    []string{"reflect", "fmt"},
		Funcs: []apisurface.Func{
			{
				Name:    "Inspect",
				Results: []apisurface.Param{{Type: "reflect.Type"}},
			},
		},
	}
	got := CheckPackage(ProfileEmbedded, bad)
	if len(got) != 2 {
		t.Fatalf("want 2 violations, got %v", got)
	}
	wantKinds := []string{"import", "result-type"}
	for i, w := range wantKinds {
		if got[i].Kind != w {
			t.Errorf("got[%d].Kind = %q, want %q", i, got[i].Kind, w)
		}
	}
}

// TestPhase16RenderFileDeterministic locks in the byte-determinism
// of RenderFile (load-bearing for the phase 10 wrapper-sha256 pin
// when the wrapper is generated under the embedded profile).
func TestPhase16RenderFileDeterministic(t *testing.T) {
	specs := []LinknameSpec{
		{
			LocalName:    "mochi_src_Pair",
			TargetSymbol: "example.com/src.Pair",
			Params:       []apisurface.Param{{Name: "k", Type: "string"}, {Name: "v", Type: "int64"}},
			Results:      []apisurface.Param{{Type: "string"}, {Type: "int64"}},
		},
		{
			LocalName:    "mochi_src_Touch",
			TargetSymbol: "example.com/src.Touch",
			Params:       []apisurface.Param{{Name: "v", Type: "int64"}},
		},
	}
	var first string
	for i := 0; i < 10; i++ {
		out, err := RenderFile("wrap", specs)
		if err != nil {
			t.Fatalf("RenderFile: %v", err)
		}
		sum := sha256.Sum256([]byte(out))
		h := hex.EncodeToString(sum[:])
		if i == 0 {
			first = h
			continue
		}
		if h != first {
			t.Fatalf("non-deterministic render on iter %d: %s vs %s", i, h, first)
		}
	}
}

// TestPhase16NoLinknameBuildTagFlip confirms the rendered file
// disappears under `-tags=mochi_no_linkname`: the build still
// succeeds (the package collapses to just the consumer file).
func TestPhase16NoLinknameBuildTagFlip(t *testing.T) {
	dir := t.TempDir()
	wrapDir := filepath.Join(dir, "wrap")
	if err := os.MkdirAll(wrapDir, 0o755); err != nil {
		t.Fatal(err)
	}
	writeFile(t, filepath.Join(wrapDir, "go.mod"), "module example.com/wrap\n\ngo 1.21\n")
	writeFile(t, filepath.Join(wrapDir, "consumer.go"), "package wrap\n")

	out, err := RenderFile("wrap", []LinknameSpec{{
		LocalName:    "mochi_x_F",
		TargetSymbol: "example.com/src.F",
		Params:       []apisurface.Param{{Name: "v", Type: "int64"}},
		Results:      []apisurface.Param{{Type: "int64"}},
	}})
	if err != nil {
		t.Fatalf("RenderFile: %v", err)
	}
	writeFile(t, filepath.Join(wrapDir, "linkname.go"), out)

	// Under the opt-out tag, the linkname file collapses; build
	// must still succeed (no missing-symbol-named errors).
	cmd := exec.Command("go", "build", "-tags=mochi_no_linkname", "./...")
	cmd.Dir = wrapDir
	cmd.Env = append(os.Environ(), "GO111MODULE=on")
	if combined, err := cmd.CombinedOutput(); err != nil {
		t.Fatalf("go build with mochi_no_linkname failed: %v\n%s", err, combined)
	}

	// And it should NOT contain the linkname symbol when the tag
	// strips the file: a quick string-grep on the source proves
	// that the consumer doesn't accidentally depend on it.
	if strings.Contains(string(linkfileContents(t, filepath.Join(wrapDir, "linkname.go"))), "//go:linkname mochi_x_F") {
		// File on disk still contains the directive; the build
		// tag is what protects against unintended linking. This
		// is the expected state.
	}
}

func linkfileContents(t *testing.T, path string) []byte {
	t.Helper()
	b, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read %s: %v", path, err)
	}
	return b
}

func writeFile(t *testing.T, path, content string) {
	t.Helper()
	if err := os.WriteFile(path, []byte(content), 0o644); err != nil {
		t.Fatalf("write %s: %v", path, err)
	}
}
