//go:build slow

package lua_test

import (
	"bytes"
	"flag"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strconv"
	"strings"
	"testing"
	"time"

	"mochi/parser"
	lua "mochi/transpiler/x/lua"
	"mochi/types"
)

// shouldUpdate reports whether -update flag was provided.
func shouldUpdate() bool {
	f := flag.Lookup("update")
	return f != nil && f.Value.String() == "true"
}

// rosettaIndex optionally selects a single program by 1-based index.
var rosettaIndex = flag.Int("index", 0, "run only the N-th Rosetta program (1-based)")

func runCase(src, outDir string) ([]byte, error) {
	base := strings.TrimSuffix(filepath.Base(src), ".mochi")
	codePath := filepath.Join(outDir, base+".lua")
	errPath := filepath.Join(outDir, base+".error")

	prog, err := parser.Parse(src)
	if err != nil {
		_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
		return nil, err
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		_ = os.WriteFile(errPath, []byte(errs[0].Error()), 0o644)
		return nil, errs[0]
	}
	ast, err := lua.Transpile(prog, env)
	if err != nil {
		_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
		return nil, err
	}
	code := lua.Emit(ast)
	if err := os.WriteFile(codePath, code, 0o644); err != nil {
		return nil, err
	}
	cmd := exec.Command("lua", codePath)
	cmd.Env = append(os.Environ(), "MOCHI_NOW_SEED=1")
	if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
		cmd.Stdin = bytes.NewReader(data)
	}
	out, err := cmd.CombinedOutput()
	if err != nil {
		_ = os.WriteFile(errPath, append([]byte(err.Error()+"\n"), out...), 0o644)
		return nil, err
	}
	_ = os.Remove(errPath)
	return bytes.TrimSpace(out), nil
}

func TestLuaTranspiler_Rosetta(t *testing.T) {
	if _, err := exec.LookPath("lua"); err != nil {
		t.Skip("lua not installed")
	}
	root := findRepoRoot(t)
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "Lua")
	os.MkdirAll(outDir, 0o755)
	t.Cleanup(updateRosettaReadme)

	pattern := filepath.Join(srcDir, "*.mochi")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	if len(files) == 0 {
		t.Fatalf("no Mochi Rosetta tests found: %s", pattern)
	}
	sort.Strings(files)
	if *rosettaIndex > 0 {
		idx := *rosettaIndex
		if idx < 1 || idx > len(files) {
			t.Fatalf("invalid -index: %d", idx)
		}
		files = []string{files[idx-1]}
	} else if only := os.Getenv("MOCHI_ROSETTA_ONLY"); only != "" {
		files = []string{filepath.Join(srcDir, only+".mochi")}
	} else if idxStr := os.Getenv("MOCHI_ROSETTA_INDEX"); idxStr != "" {
		idx, err := strconv.Atoi(idxStr)
		if err != nil || idx < 1 || idx > len(files) {
			t.Fatalf("invalid MOCHI_ROSETTA_INDEX: %s", idxStr)
		}
		files = []string{files[idx-1]}
	}

	var passed int
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		ok := t.Run(name, func(t *testing.T) {
			got, err := runCase(src, outDir)
			if err != nil {
				t.Fatalf("run error: %v", err)
			}
			wantPath := filepath.Join(outDir, name+".out")
			want, err := os.ReadFile(wantPath)
			if err != nil {
				if shouldUpdate() {
					if werr := os.WriteFile(wantPath, append(got, '\n'), 0o644); werr == nil {
						t.Logf("updated: %s", wantPath)
						return
					} else {
						t.Fatalf("write golden: %v", werr)
					}
				}
				t.Fatalf("read golden: %v", err)
			}
			got = bytes.TrimSpace(got)
			want = bytes.TrimSpace(want)
			if !bytes.Equal(got, want) {
				if shouldUpdate() {
					if werr := os.WriteFile(wantPath, append(got, '\n'), 0o644); werr == nil {
						t.Logf("updated: %s", wantPath)
						return
					} else {
						t.Fatalf("write golden: %v", werr)
					}
				}
				t.Errorf("golden mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", name, got, want)
			}
		})
		if !ok {
			t.Fatalf("first failing program: %s", name)
		}
		passed++
	}
	t.Logf("Summary: %d passed", passed)
}

func updateRosettaReadme() {
	root := findRepoRoot(&testing.T{})
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "Lua")
	readmePath := filepath.Join(root, "transpiler", "x", "lua", "ROSETTA.md")

	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	sort.Strings(files)
	total := len(files)
	compiled := 0
	var lines []string
	for i, f := range files {
		name := strings.TrimSuffix(filepath.Base(f), ".mochi")
		mark := "[ ]"
		if _, err := os.Stat(filepath.Join(outDir, name+".out")); err == nil {
			if _, err2 := os.Stat(filepath.Join(outDir, name+".error")); os.IsNotExist(err2) {
				compiled++
				mark = "[x]"
			}
		}
		lines = append(lines, fmt.Sprintf("%d. %s %s", i+1, mark, name))
	}
	ts := ""
	if out, err := exec.Command("git", "log", "-1", "--format=%cI").Output(); err == nil {
		if t, perr := time.Parse(time.RFC3339, strings.TrimSpace(string(out))); perr == nil {
			loc := time.FixedZone("GMT+7", 7*3600)
			ts = t.In(loc).Format("2006-01-02 15:04 MST")
		}
	}
	var buf bytes.Buffer
	buf.WriteString("# Lua Rosetta Transpiler Output\n\n")
	buf.WriteString("Generated Lua code for programs in `tests/rosetta/x/Mochi`. Each program has a `.lua` file produced by the transpiler and a `.out` file with its runtime output. Compilation or runtime errors are captured in `.error` files.\n\n")
	fmt.Fprintf(&buf, "Transpiled programs: %d/%d\n\n", compiled, total)
	if ts != "" {
		buf.WriteString("Last updated: " + ts + "\n\n")
	}
	buf.WriteString("Checklist:\n\n")
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
}
