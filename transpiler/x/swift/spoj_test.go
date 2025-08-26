//go:build slow

package swifttrans_test

import (
	"bufio"
	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strconv"
	"strings"
	"testing"
	"time"

	"mochi/parser"
	swifttrans "mochi/transpiler/x/swift"
	"mochi/types"
)

func TestSwiftTranspiler_SPOJ_Golden(t *testing.T) {
	swiftExe := ensureSwift(t)
	root := repoRoot()
	t.Cleanup(updateSPOJ)

	idx := 1
	if s := os.Getenv("MOCHI_SPOJ_INDEX"); s != "" {
		n, err := strconv.Atoi(s)
		if err != nil || n < 1 {
			t.Fatalf("invalid MOCHI_SPOJ_INDEX: %s", s)
		}
		idx = n
	}

	src := filepath.Join(root, "tests", "spoj", "x", "mochi", fmt.Sprintf("%d.mochi", idx))
	outDir := filepath.Join(root, "tests", "spoj", "x", "swift")
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		t.Fatalf("mkout: %v", err)
	}
	base := strconv.Itoa(idx)
	codePath := filepath.Join(outDir, base+".swift")
	outPath := filepath.Join(outDir, base+".out")
	errPath := filepath.Join(outDir, base+".error")
	benchPath := filepath.Join(outDir, base+".bench")
	inPath := filepath.Join(outDir, base+".in")

	bench := os.Getenv("MOCHI_BENCHMARK") == "1" || os.Getenv("MOCHI_BENCHMARK") == "true"

	var want []byte
	if data, err := os.ReadFile(outPath); err == nil {
		want = data
		if !bench {
			want = bytes.TrimSpace(want)
		}
	}

	prog, err := parser.Parse(src)
	if err != nil {
		_ = os.WriteFile(errPath, []byte("parse: "+err.Error()), 0o644)
		t.Fatalf("parse: %v", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		_ = os.WriteFile(errPath, []byte("type: "+errs[0].Error()), 0o644)
		t.Fatalf("type: %v", errs[0])
	}
	ast, err := swifttrans.Transpile(env, prog, bench)
	if err != nil {
		_ = os.WriteFile(errPath, []byte("transpile: "+err.Error()), 0o644)
		t.Fatalf("transpile: %v", err)
	}
	code := ast.Emit()
	if err := os.WriteFile(codePath, code, 0o644); err != nil {
		t.Fatalf("write code: %v", err)
	}

	inData, _ := os.ReadFile(inPath)
	out, err := compileAndRunSwiftSrc(t, swiftExe, code, inData, bench)
	got := bytes.TrimSpace(out)
	if err != nil {
		_ = os.WriteFile(errPath, append([]byte("run: "+err.Error()+"\n"), out...), 0o644)
		t.Fatalf("run: %v", err)
	}
	_ = os.Remove(errPath)

	if bench {
		idx := bytes.LastIndexByte(got, '{')
		if idx >= 0 {
			part := bytes.TrimSpace(got[idx:])
			if json.Valid(part) {
				_ = os.WriteFile(benchPath, part, 0o644)
			} else {
				_ = os.WriteFile(benchPath, got, 0o644)
			}
		} else {
			_ = os.WriteFile(benchPath, got, 0o644)
		}
		return
	}

	if want == nil {
		_ = os.WriteFile(outPath, append(got, '\n'), 0o644)
		return
	}
	if !bytes.Equal(got, want) {
		_ = os.WriteFile(errPath, []byte(fmt.Sprintf("output mismatch\n-- got --\n%s\n-- want --\n%s", got, want)), 0o644)
		t.Fatalf("output mismatch")
	}
}

func updateSPOJ() {
	root := repoRoot()
	srcDir := filepath.Join(root, "tests", "spoj", "x", "mochi")
	outDir := filepath.Join(root, "tests", "spoj", "x", "swift")
	md := filepath.Join(root, "transpiler", "x", "swift", "SPOJ.md")

	files, _ := filepath.Glob(filepath.Join(srcDir, "*.mochi"))
	sort.Slice(files, func(i, j int) bool {
		ai, _ := strconv.Atoi(strings.TrimSuffix(filepath.Base(files[i]), ".mochi"))
		aj, _ := strconv.Atoi(strings.TrimSuffix(filepath.Base(files[j]), ".mochi"))
		return ai < aj
	})

	total := len(files)
	compiled := 0
	var rows []string
	for _, src := range files {
		base := strings.TrimSuffix(filepath.Base(src), ".mochi")
		status := " "
		dur := ""
		mem := ""
		if _, err := os.Stat(filepath.Join(outDir, base+".error")); err == nil {
			status = "error"
		} else if _, err := os.Stat(filepath.Join(outDir, base+".swift")); err == nil {
			status = "✓"
			compiled++
			if data, err := os.ReadFile(filepath.Join(outDir, base+".bench")); err == nil {
				var r struct {
					DurationUS  int64 `json:"duration_us"`
					MemoryBytes int64 `json:"memory_bytes"`
				}
				if json.Unmarshal(bytes.TrimSpace(data), &r) == nil {
					dur = formatDuration(time.Duration(r.DurationUS) * time.Microsecond)
					mem = formatBytes(r.MemoryBytes)
				}
			}
		}
		name := parseSpojName(src)
		rows = append(rows, fmt.Sprintf("| %s | %s | %s | %s | %s |", base, name, status, dur, mem))
	}
	var buf bytes.Buffer
	buf.WriteString("# SPOJ Transpiler Progress\n\n")
	buf.WriteString("This checklist is auto-generated.\n")
	buf.WriteString("Generated Swift code from programs in `tests/spoj/x/mochi` lives in `tests/spoj/x/swift`.\n")
	loc := time.FixedZone("GMT+7", 7*60*60)
	buf.WriteString("Last updated: " + time.Now().In(loc).Format("2006-01-02 15:04 MST") + "\n\n")
	buf.WriteString(fmt.Sprintf("## SPOJ Golden Test Checklist (%d/%d)\n", compiled, total))
	buf.WriteString("| Index | Name | Status | Duration | Memory |\n")
	buf.WriteString("|------:|------|:-----:|---------:|-------:|\n")
	for _, row := range rows {
		buf.WriteString(row + "\n")
	}
	_ = os.WriteFile(md, buf.Bytes(), 0o644)
}

func parseSpojName(src string) string {
	mdPath := strings.TrimSuffix(src, ".mochi") + ".md"
	data, err := os.ReadFile(mdPath)
	if err != nil {
		return strings.TrimSuffix(filepath.Base(src), ".mochi")
	}
	scanner := bufio.NewScanner(bytes.NewReader(data))
	for scanner.Scan() {
		line := scanner.Text()
		if idx := strings.Index(line, "problems/"); idx >= 0 {
			rest := line[idx+len("problems/"):]
			if j := strings.Index(rest, ")"); j >= 0 {
				name := rest[:j]
				return strings.TrimSuffix(name, "/")
			}
		}
	}
	return strings.TrimSuffix(filepath.Base(src), ".mochi")
}

func formatDuration(d time.Duration) string {
	switch {
	case d < time.Microsecond:
		return fmt.Sprintf("%dns", d.Nanoseconds())
	case d < time.Millisecond:
		return fmt.Sprintf("%.1fµs", float64(d.Microseconds()))
	case d < time.Second:
		return fmt.Sprintf("%.1fms", float64(d.Milliseconds()))
	default:
		return fmt.Sprintf("%.2fs", d.Seconds())
	}
}

func formatBytes(n int64) string {
	const (
		_KB = 1024
		_MB = _KB * 1024
		_GB = _MB * 1024
	)
	switch {
	case n >= _GB:
		return fmt.Sprintf("%.2fGB", float64(n)/float64(_GB))
	case n >= _MB:
		return fmt.Sprintf("%.2fMB", float64(n)/float64(_MB))
	case n >= _KB:
		return fmt.Sprintf("%.2fKB", float64(n)/float64(_KB))
	default:
		return fmt.Sprintf("%dB", n)
	}
}
