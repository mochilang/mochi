//go:build slow

package any2mochi

import (
	"bytes"
	"os"
	"path/filepath"
	"testing"

	tscode "mochi/compile/ts"
)

func TestParseTypeScript_HelloWorld_Golden(t *testing.T) {
	root := findRepoRoot(t)
	_ = tscode.EnsureTSLanguageServer()
	src := filepath.Join(root, "tests/any2mochi_parse/ts/hello.ts")
	outPath := filepath.Join(root, "tests/any2mochi_parse/ts/hello.ts.json")

	out, err := ParseTypeScriptFileJSON(src)
	if err != nil {
		t.Fatalf("parse: %v", err)
	}

	if *update {
		os.WriteFile(outPath, out, 0644)
	}
	want, err := os.ReadFile(outPath)
	if err != nil {
		t.Fatalf("missing golden output: %v", err)
	}
	if !bytes.Equal(bytes.TrimSpace(out), bytes.TrimSpace(want)) {
		t.Errorf("golden mismatch\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", out, want)
	}
}
