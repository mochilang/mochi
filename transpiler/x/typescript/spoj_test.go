//go:build slow

package typescript_test

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"testing"
	"time"

	"mochi/parser"
	meta "mochi/transpiler/meta"
	tstranspiler "mochi/transpiler/x/ts"
	"mochi/types"
)

func ensureDeno(t *testing.T) {
	if err := meta.EnsureDeno(); err != nil {
		t.Skipf("deno not installed: %v", err)
	}
}

func repoRoot(t *testing.T) string {
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

func TestTypescriptTranspiler_SPOJ_Golden(t *testing.T) {
	ensureDeno(t)
	root := repoRoot(t)
	t.Cleanup(updateSPOJ)

	src := filepath.Join(root, "tests", "spoj", "x", "mochi", "1.mochi")
	outDir := filepath.Join(root, "tests", "spoj", "x", "typescript")
	os.MkdirAll(outDir, 0o755)
	codePath := filepath.Join(outDir, "1.ts")
	outPath := filepath.Join(outDir, "1.out")
	errPath := filepath.Join(outDir, "1.error")

	bench := os.Getenv("MOCHI_BENCHMARK") != ""

	want, err := os.ReadFile(outPath)
	if err != nil {
		t.Fatalf("read want: %v", err)
	}
	if !bench {
		want = bytes.TrimSpace(want)
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
	tsprog, err := tstranspiler.Transpile(prog, env, bench)
	if err != nil {
		_ = os.WriteFile(errPath, []byte("transpile: "+err.Error()), 0o644)
		t.Fatalf("transpile: %v", err)
	}
	code := tstranspiler.Emit(tsprog)
	if err := os.WriteFile(codePath, code, 0o644); err != nil {
		t.Fatalf("write code: %v", err)
	}

	cmd := exec.Command("deno", "run", "--quiet", "--allow-read", "--allow-env", codePath)
	envv := append(os.Environ(), "DENO_TLS_CA_STORE=system", "DENO_INSTALLER=skip", "DENO_NO_UPDATE_CHECK=1")
	if !bench {
		envv = append(envv, "MOCHI_NOW_SEED=1")
	}
	cmd.Env = envv
	inPath := filepath.Join(outDir, "1.in")
	if data, err := os.ReadFile(inPath); err == nil {
		cmd.Stdin = bytes.NewReader(data)
	}
	out, err := cmd.CombinedOutput()
	got := bytes.TrimSpace(out)
	if err != nil {
		_ = os.WriteFile(errPath, append([]byte("run: "+err.Error()+"\n"), out...), 0o644)
		t.Fatalf("run: %v", err)
	}
	_ = os.Remove(errPath)

	if bench {
		benchPath := filepath.Join(outDir, "1.bench")
		idx := bytes.LastIndex(got, []byte("{"))
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

	if !bytes.Equal(got, want) {
		t.Fatalf("output mismatch\n\n--- Got ---\n%s\n\n--- Want ---\n%s", got, want)
	}
}

func updateSPOJ() {
	root := repoRoot(&testing.T{})
	outDir := filepath.Join(root, "tests", "spoj", "x", "typescript")
	md := filepath.Join(root, "transpiler", "x", "typescript", "SPOJ.md")
	status := " "
	dur := ""
	mem := ""
	if _, err := os.Stat(filepath.Join(outDir, "1.error")); err == nil {
		status = "error"
	} else if _, err := os.Stat(filepath.Join(outDir, "1.ts")); err == nil {
		status = "✓"
	}
	if data, err := os.ReadFile(filepath.Join(outDir, "1.bench")); err == nil {
		var r struct {
			DurationUS  int64 `json:"duration_us"`
			MemoryBytes int64 `json:"memory_bytes"`
		}
		if json.Unmarshal(bytes.TrimSpace(data), &r) == nil {
			dur = formatDuration(time.Duration(r.DurationUS) * time.Microsecond)
			mem = formatBytes(r.MemoryBytes)
		}
	}
	compiled := 0
	if status == "✓" {
		compiled = 1
	}
	var buf bytes.Buffer
	buf.WriteString("# SPOJ Transpiler Progress\n\n")
	buf.WriteString("This checklist is auto-generated.\n")
	buf.WriteString("Generated TypeScript code from programs in `tests/spoj/x/mochi` lives in `tests/spoj/x/typescript`.\n")
	loc := time.FixedZone("GMT+7", 7*60*60)
	buf.WriteString("Last updated: " + time.Now().In(loc).Format("2006-01-02 15:04 MST") + "\n\n")
	buf.WriteString("## SPOJ Golden Test Checklist (" + strconv.Itoa(compiled) + "/1)\n")
	buf.WriteString("| Index | Name | Status | Duration | Memory |\n")
	buf.WriteString("|------:|------|:-----:|---------:|-------:|\n")
	buf.WriteString("| 1 | Life, the Universe, and Everything | " + status + " | " + dur + " | " + mem + " |\n")
	_ = os.WriteFile(md, buf.Bytes(), 0o644)
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
