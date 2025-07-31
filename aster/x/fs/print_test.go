//go:build slow

package fs_test

import (
	"encoding/json"
	"flag"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
	"testing"

	fs "mochi/aster/x/fs"
)

func shouldUpdate() bool {
	f := flag.Lookup("update")
	return f != nil && f.Value.String() == "true"
}

func ensureFSharp(t *testing.T) {
	if _, err := exec.LookPath("fsharpc"); err != nil {
		t.Skip("fsharpc not installed")
	}
}

func TestPrint_Golden(t *testing.T) {
	ensureFSharp(t)
	root := repoRoot(t)
	srcDir := filepath.Join(root, "tests", "transpiler", "x", "fs")
	outDir := filepath.Join(root, "tests", "aster", "x", "fs")
	os.MkdirAll(outDir, 0o755)

	files, err := filepath.Glob(filepath.Join(srcDir, "*.fs"))
	if err != nil {
		t.Fatal(err)
	}
	sort.Strings(files)

	var selected []string
	for _, f := range files {
		base := strings.TrimSuffix(filepath.Base(f), ".fs")
		if _, err := os.Stat(filepath.Join(srcDir, base+".out")); err == nil {
			selected = append(selected, f)
		}
	}
	files = selected
	if len(files) > 10 {
		files = files[:10]
	}

	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".fs")
		t.Run(name, func(t *testing.T) {
			data, err := os.ReadFile(src)
			if err != nil {
				t.Fatalf("read src: %v", err)
			}
			prog, err := fs.Inspect(string(data))
			if err != nil {
				t.Fatalf("inspect: %v", err)
			}
			astJSON, err := json.MarshalIndent(prog, "", "  ")
			if err != nil {
				t.Fatalf("marshal: %v", err)
			}
			astJSON = append(astJSON, '\n')
			jsonPath := filepath.Join(outDir, name+".fs.json")
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
			out, err := fs.Print(prog)
			if err != nil {
				t.Fatalf("print: %v", err)
			}
			outPath := filepath.Join(outDir, name+".fs")
			if shouldUpdate() {
				if err := os.WriteFile(outPath, []byte(out), 0644); err != nil {
					t.Fatalf("write out: %v", err)
				}
			}
			exe := filepath.Join(outDir, name+".exe")
			cmd := exec.Command("fsharpc", "--target:exe", "--out:"+exe, outPath)
			if b, err := cmd.CombinedOutput(); err != nil {
				t.Fatalf("compile printed: %v\n%s", err, b)
			}
			run := exec.Command("mono", exe)
			got, err := run.CombinedOutput()
			if err != nil {
				t.Fatalf("run printed: %v\n%s", err, got)
			}
			cmdOrig := exec.Command("fsharpc", "--target:exe", "--out:"+exe, src)
			if b, err := cmdOrig.CombinedOutput(); err != nil {
				t.Fatalf("compile original: %v\n%s", err, b)
			}
			runOrig := exec.Command("mono", exe)
			want, err := runOrig.CombinedOutput()
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
