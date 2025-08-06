//go:build slow

package swifttrans_test

import (
	"bufio"
	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
	"time"

	"mochi/parser"
	swifttrans "mochi/transpiler/x/swift"
	"mochi/types"
)

func TestSwiftTranspiler_Algorithms_Golden(t *testing.T) {
	swiftExe := ensureSwift(t)
	root := repoRoot()
	srcDir := filepath.Join(root, "tests", "github", "TheAlgorithms", "Mochi")
	outDir := filepath.Join(root, "tests", "algorithms", "transpiler", "Swift")
	os.MkdirAll(outDir, 0o755)
	t.Cleanup(updateAlgorithmsChecklist)

	names, err := readAlgIndex(filepath.Join(srcDir, "index.txt"))
	if err != nil {
		t.Fatalf("read index: %v", err)
	}
	if len(names) == 0 {
		t.Fatalf("no mochi files in %s", srcDir)
	}
	idx := 1
	if v := os.Getenv("MOCHI_ALGORITHMS_INDEX"); v != "" {
		n, err := strconv.Atoi(v)
		if err != nil || n < 1 || n > len(names) {
			t.Fatalf("invalid MOCHI_ALGORITHMS_INDEX: %s", v)
		}
		idx = n
	}
	files := []string{filepath.Join(srcDir, names[idx-1])}
	rels := []string{names[idx-1]}

	for i, src := range files {
		rel := strings.TrimSuffix(rels[i], ".mochi")
		name := strings.ReplaceAll(rel, string(os.PathSeparator), "_")
		testName := fmt.Sprintf("%03d_%s", idx, name)
		t.Run(testName, func(t *testing.T) {
			codePath := filepath.Join(outDir, rel+".swift")
			outPath := filepath.Join(outDir, rel+".out")
			errPath := filepath.Join(outDir, rel+".error")
			benchPath := filepath.Join(outDir, rel+".bench")
			os.MkdirAll(filepath.Dir(codePath), 0o755)

			prog, err := parser.Parse(src)
			if err != nil {
				_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
				t.Fatalf("parse: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				_ = os.WriteFile(errPath, []byte(errs[0].Error()), 0o644)
				t.Fatalf("type: %v", errs[0])
			}
			bench := true
			ast, err := swifttrans.Transpile(env, prog, bench)
			if err != nil {
				_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
				t.Fatalf("transpile: %v", err)
			}
			code := ast.Emit()
			if err := os.WriteFile(codePath, code, 0o644); err != nil {
				t.Fatalf("write code: %v", err)
			}
			inData, _ := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in")

			out, err := compileAndRunSwiftSrc(t, swiftExe, code, inData, bench)
			if err != nil {
				_ = os.WriteFile(errPath, append([]byte(err.Error()+"\n"), out...), 0o644)
				t.Fatalf("run: %v", err)
			}
			_ = os.Remove(errPath)
			got := bytes.TrimSpace(out)
			benchData := got
			if idx := bytes.LastIndexByte(got, '{'); idx >= 0 {
				_ = os.WriteFile(outPath, bytes.TrimSpace(got[:idx]), 0o644)
				benchData = got[idx:]
			} else {
				_ = os.WriteFile(outPath, got, 0o644)
				benchData = nil
			}
			if benchData != nil {
				_ = os.WriteFile(benchPath, benchData, 0o644)
			}
		})
	}
}

func readAlgIndex(path string) ([]string, error) {
	f, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer f.Close()
	var names []string
	scanner := bufio.NewScanner(f)
	for scanner.Scan() {
		parts := strings.Fields(scanner.Text())
		if len(parts) == 2 {
			names = append(names, parts[1])
		}
	}
	if err := scanner.Err(); err != nil {
		return nil, err
	}
	return names, nil
}

func updateAlgorithmsChecklist() {
	root := repoRoot()
	srcDir := filepath.Join(root, "tests", "github", "TheAlgorithms", "Mochi")
	outDir := filepath.Join(root, "tests", "algorithms", "transpiler", "Swift")
	docPath := filepath.Join(root, "transpiler", "x", "swift", "ALGORITHMS.md")
	names, _ := readAlgIndex(filepath.Join(srcDir, "index.txt"))
	total := len(names)
	compiled := 0
	var lines []string
	lines = append(lines, "| Index | Name | Status | Duration | Memory |")
	lines = append(lines, "|------:|------|--------|---------:|-------:|")
	for i, f := range names {
		name := strings.TrimSuffix(f, ".mochi")
		status := " "
		dur := ""
		mem := ""
		if _, err := os.Stat(filepath.Join(outDir, name+".error")); err == nil {
			status = " "
		} else if _, err := os.Stat(filepath.Join(outDir, name+".swift")); err == nil {
			status = "✓"
			compiled++
		}
		if data, err := os.ReadFile(filepath.Join(outDir, name+".bench")); err == nil {
			var r struct {
				DurationUS  int64 `json:"duration_us"`
				MemoryBytes int64 `json:"memory_bytes"`
			}
			data = bytes.TrimSpace(data)
			if json.Unmarshal(data, &r) == nil && r.DurationUS > 0 {
				dur = humanDuration(r.DurationUS)
				mem = humanSize(r.MemoryBytes)
			}
		}
		lines = append(lines, fmt.Sprintf("| %d | %s | %s | %s | %s |", i+1, name, status, dur, mem))
	}
	tsRaw, _ := exec.Command("git", "log", "-1", "--format=%cI").Output()
	tsStr := strings.TrimSpace(string(tsRaw))
	if t, err := time.Parse(time.RFC3339, tsStr); err == nil {
		if loc, lerr := time.LoadLocation("Asia/Bangkok"); lerr == nil {
			tsStr = t.In(loc).Format("2006-01-02 15:04 -0700")
		} else {
			tsStr = t.Format("2006-01-02 15:04 MST")
		}
	} else {
		tsStr = time.Now().Format("2006-01-02 15:04 MST")
	}
	var buf bytes.Buffer
	buf.WriteString("# Swift Algorithms Transpiler Output\n\n")
	buf.WriteString("Generated Swift code for algorithms in `tests/github/TheAlgorithms/Mochi`. Outputs are stored in `tests/algorithms/transpiler/Swift`. Errors are captured in `.error` files.\n\n")
	fmt.Fprintf(&buf, "Completed programs: %d/%d\n", compiled, total)
	buf.WriteString(fmt.Sprintf("Last updated: %s\n\n", tsStr))
	buf.WriteString(strings.Join(lines, "\n"))
	buf.WriteString("\n")
	_ = os.WriteFile(docPath, buf.Bytes(), 0o644)
}
