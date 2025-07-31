//go:build slow

package java_test

import (
	"encoding/json"
	"flag"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
	"testing"

	javaast "mochi/aster/x/java"
)

func shouldUpdate() bool {
	f := flag.Lookup("update")
	return f != nil && f.Value.String() == "true"
}

func TestPrint_Golden(t *testing.T) {
	ensureJava(t)
	root := repoRoot(t)
	srcDir := filepath.Join(root, "tests", "transpiler", "x", "java")
	outDir := filepath.Join(root, "tests", "aster", "x", "java")
	os.MkdirAll(outDir, 0o755)

	files, err := filepath.Glob(filepath.Join(srcDir, "*.java"))
	if err != nil {
		t.Fatal(err)
	}
	sort.Strings(files)

	allowed := map[string]bool{
		"append_builtin":    true,
		"avg_builtin":       true,
		"basic_compare":     true,
		"bench_block":       true,
		"binary_precedence": true,
		"group_by_having":   true,
	}
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".java")
		if !allowed[name] {
			continue
		}
		if _, err := os.Stat(filepath.Join(srcDir, name+".error")); err == nil {
			t.Logf("skip %s due to compile error", name)
			continue
		}
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(src)
			if err != nil {
				t.Fatalf("read src: %v", err)
			}
			prog, err := javaast.Inspect(string(data), javaast.Options{})
			if err != nil {
				t.Fatalf("inspect: %v", err)
			}
			astJSON, err := json.MarshalIndent(prog, "", "  ")
			if err != nil {
				t.Fatalf("marshal: %v", err)
			}
			astJSON = append(astJSON, '\n')
			jsonPath := filepath.Join(outDir, name+".java.json")
			if shouldUpdate() {
				if err := os.WriteFile(jsonPath, astJSON, 0644); err != nil {
					t.Fatalf("write json: %v", err)
				}
			}
			wantJSON, err := os.ReadFile(jsonPath)
			if err != nil {
				t.Skip("missing golden")
				return
			}
			if string(astJSON) != string(wantJSON) {
				t.Fatalf("json mismatch\n--- got ---\n%s\n--- want ---\n%s", astJSON, wantJSON)
			}
			out, err := javaast.Print(prog)
			if err != nil {
				t.Fatalf("print: %v", err)
			}
			outPath := filepath.Join(outDir, name+".java")
			if shouldUpdate() {
				if err := os.WriteFile(outPath, []byte(out), 0644); err != nil {
					t.Fatalf("write out: %v", err)
				}
			}
			tmp := t.TempDir()
			file := filepath.Join(tmp, "Main.java")
			if err := os.WriteFile(file, []byte(out), 0644); err != nil {
				t.Fatalf("write temp: %v", err)
			}
			if outc, err := exec.Command("javac", "-d", tmp, file).CombinedOutput(); err != nil {
				t.Fatalf("javac printed: %v\n%s", err, outc)
			}
			got, err := exec.Command("java", "-cp", tmp, "Main").CombinedOutput()
			if err != nil {
				t.Fatalf("run printed: %v\n%s", err, got)
			}
			tmp2 := t.TempDir()
			orig := filepath.Join(tmp2, "Main.java")
			data2, err := os.ReadFile(src)
			if err != nil {
				t.Fatalf("read src: %v", err)
			}
			if err := os.WriteFile(orig, data2, 0644); err != nil {
				t.Fatalf("write temp src: %v", err)
			}
			if outc, err := exec.Command("javac", "-d", tmp2, orig).CombinedOutput(); err != nil {
				t.Fatalf("javac original: %v\n%s", err, outc)
			}
			want, err := exec.Command("java", "-cp", tmp2, "Main").CombinedOutput()
			if err != nil {
				t.Fatalf("run original: %v\n%s", err, want)
			}
			outFile := filepath.Join(outDir, name+".out")
			if shouldUpdate() {
				if err := os.WriteFile(outFile, got, 0644); err != nil {
					t.Fatalf("write out file: %v", err)
				}
			}
			if string(got) != string(want) {
				t.Fatalf("output mismatch\n--- got ---\n%s\n--- want ---\n%s", got, want)
			}
		})
	}
}
