//go:build slow

package pl_test

import (
	"bytes"
	"encoding/json"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"testing"

	"mochi/parser"
	pl "mochi/transpiler/x/pl"
	"mochi/types"
)

func repoRootRosetta(t *testing.T) string {
	dir, err := os.Getwd()
	if err != nil {
		t.Fatal("cannot determine working directory")
	}
	for i := 0; i < 10; i++ {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			break
		}
		dir = parent
	}
	t.Fatal("go.mod not found")
	return ""
}

func shouldUpdateRosetta() bool {
	if v, ok := os.LookupEnv("UPDATE"); ok && (v == "1" || v == "true") {
		return true
	}
	return false
}

func runRosettaTask(t *testing.T, name string) {
	root := repoRootRosetta(t)
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "Prolog")
	os.MkdirAll(outDir, 0o755)

	outPath := filepath.Join(outDir, name+".out")
	benchPath := filepath.Join(outDir, name+".bench")
	errPath := filepath.Join(outDir, name+".error")

	wantPath := filepath.Join(root, "tests", "rosetta", "x", "Mochi", name+".out")
	os.Setenv("ROSETTA_OUT_PATH", wantPath)
	defer os.Unsetenv("ROSETTA_OUT_PATH")

	src := filepath.Join(root, "tests", "rosetta", "x", "Mochi", name+".mochi")
	prog, err := parser.Parse(src)
	if err != nil {
		t.Fatalf("%s: parse: %v", name, err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("%s: type: %v", name, errs[0])
	}
	bench := os.Getenv("MOCHI_BENCHMARK") != ""
	pl.SetBenchMain(bench)
	ast, err := pl.Transpile(prog, env)
	if err != nil {
		t.Fatalf("%s: transpile: %v", name, err)
	}
	var buf bytes.Buffer
	if err := pl.Emit(&buf, ast); err != nil {
		t.Fatalf("%s: emit: %v", name, err)
	}
	plFile := filepath.Join(outDir, name+".pl")
	if err := os.WriteFile(plFile, buf.Bytes(), 0o644); err != nil {
		t.Fatalf("%s: write: %v", name, err)
	}
	cmd := exec.Command("swipl", "-q", "-f", plFile)
	cmd.Env = append(os.Environ(), "MOCHI_ROOT="+root)
	out, err := cmd.CombinedOutput()
	got := bytes.TrimSpace(out)
	if err != nil || bytes.Contains(out, []byte("ERROR:")) {
		_ = os.WriteFile(errPath, out, 0o644)
		t.Fatalf("%s: run: %v\n%s", name, err, string(out))
	}
	_ = os.Remove(errPath)
	if bench {
		_ = os.WriteFile(benchPath, got, 0o644)
		var js struct {
			Duration int64  `json:"duration_us"`
			Memory   int64  `json:"memory_bytes"`
			Name     string `json:"name"`
		}
		_ = json.Unmarshal(got, &js)
		return
	}
	_ = os.WriteFile(outPath, got, 0o644)
	if shouldUpdateRosetta() {
		_ = os.WriteFile(outPath, append(got, '\n'), 0o644)
		return
	}
	if want, err := os.ReadFile(outPath); err == nil {
		if !bytes.Equal(got, bytes.TrimSpace(want)) {
			t.Fatalf("%s: output mismatch:\nGot: %s\nWant: %s", name, got, bytes.TrimSpace(want))
		}
	}
}

func TestPrologTranspiler_Rosetta(t *testing.T) {
	if _, err := exec.LookPath("swipl"); err != nil {
		t.Skip("swipl not installed")
	}
	root := repoRootRosetta(t)
	idxPath := filepath.Join(root, "tests", "rosetta", "x", "Mochi", "index.txt")
	data, err := os.ReadFile(idxPath)
	if err != nil {
		t.Fatalf("read index: %v", err)
	}
	var files []string
	for _, line := range strings.Split(strings.TrimSpace(string(data)), "\n") {
		fields := strings.Fields(line)
		if len(fields) >= 2 {
			files = append(files, filepath.Join(root, "tests", "rosetta", "x", "Mochi", fields[1]))
		}
	}

	if v := os.Getenv("ROSETTA_INDEX"); v != "" {
		idx, err := strconv.Atoi(v)
		if err != nil || idx < 1 || idx > len(files) {
			t.Fatalf("invalid ROSETTA_INDEX: %s", v)
		}
		files = files[idx-1 : idx]
	} else {
		max := len(files)
		if v := os.Getenv("ROSETTA_MAX"); v != "" {
			if n, err := strconv.Atoi(v); err == nil && n < max {
				max = n
			}
		}
		files = files[:max]
	}

	var firstFail string
	for _, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		ok := t.Run(name, func(t *testing.T) { runRosettaTask(t, name) })
		if !ok {
			firstFail = name
			break
		}
	}
	pl.UpdateRosettaReadme()
	if firstFail != "" {
		t.Fatalf("first failing program: %s", firstFail)
	}
}
