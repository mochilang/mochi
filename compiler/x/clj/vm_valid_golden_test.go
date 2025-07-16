//go:build slow

package cljcode_test

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	cljcode "mochi/compiler/x/clj"
	"mochi/parser"
	"mochi/types"
)

func stripHeaderValid(b []byte) []byte {
	if i := bytes.IndexByte(b, '\n'); i != -1 && bytes.HasPrefix(b, []byte("; Generated")) {
		return b[i+1:]
	}
	return b
}

func TestClojureCompiler_VMValid_Golden(t *testing.T) {
	if err := cljcode.EnsureClojure(); err != nil {
		t.Skipf("clojure not installed: %v", err)
	}
	root := findRepoRoot(t)
	srcDir := filepath.Join(root, "tests", "vm", "valid")
	goldenDir := filepath.Join(root, "tests", "machine", "x", "clj")
	files, err := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	if err != nil {
		t.Fatalf("glob error: %v", err)
	}
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		t.Run(name, func(t *testing.T) {
			prog, err := parser.Parse(src)
			if err != nil {
				t.Fatalf("parse error: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				t.Fatalf("type error: %v", errs[0])
			}
			code, err := cljcode.New(env).Compile(prog)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			wantCode, err := os.ReadFile(filepath.Join(goldenDir, name+".clj"))
			if err != nil {
				t.Fatalf("read golden code: %v", err)
			}
			got := stripHeaderValid(bytes.TrimSpace(code))
			want := stripHeaderValid(bytes.TrimSpace(wantCode))
			if !bytes.Equal(got, want) {
				t.Errorf("generated code mismatch for %s.clj\n\n--- Got ---\n%s\n\n--- Want ---\n%s", name, got, want)
			}
			dir := t.TempDir()
			file := filepath.Join(dir, "main.clj")
			if err := os.WriteFile(file, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			cmd := exec.Command("clojure", file)
			if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
				cmd.Stdin = bytes.NewReader(data)
			}
			cmd.Env = append(os.Environ(), "CLASSPATH=/usr/share/java/data.json.jar:/usr/share/java/snakeyaml-engine.jar")
			out, err := cmd.CombinedOutput()
			if err != nil {
				if _, err2 := os.Stat(filepath.Join(goldenDir, name+".error")); err2 == nil {
					return
				}
				t.Fatalf("clojure run error: %v\n%s", err, out)
			}
			gotOut := bytes.TrimSpace(out)
			outWant, err := os.ReadFile(filepath.Join(goldenDir, name+".out"))
			if err != nil {
				t.Fatalf("read golden output: %v", err)
			}
			if !bytes.Equal(gotOut, bytes.TrimSpace(outWant)) {
				t.Errorf("output mismatch for %s.out\n\n--- Got ---\n%s\n\n--- Want ---\n%s", name, gotOut, bytes.TrimSpace(outWant))
			}
		})
	}
}
