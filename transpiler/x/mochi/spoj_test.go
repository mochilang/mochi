//go:build slow

package mochi_test

import (
    "bytes"
    "encoding/json"
    "fmt"
    "os"
    "path/filepath"
    "testing"
    "time"

    "mochi/parser"
    vm "mochi/runtime/vm"
    "mochi/types"
)

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

func TestMochiTranspiler_SPOJ_Golden(t *testing.T) {
    root := repoRoot(t)
    t.Cleanup(updateSPOJ)

    src := filepath.Join(root, "tests", "spoj", "x", "mochi", "15.mochi")
    outDir := filepath.Join(root, "tests", "spoj", "x", "mochi")
    outPath := filepath.Join(outDir, "15.out")
    errPath := filepath.Join(outDir, "15.error")

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
    p, err := vm.Compile(prog, env)
    if err != nil {
        _ = os.WriteFile(errPath, []byte("compile: "+err.Error()), 0o644)
        t.Fatalf("compile: %v", err)
    }
    inPath := filepath.Join(outDir, "15.in")
    data, _ := os.ReadFile(inPath)
    var out bytes.Buffer
    m := vm.NewWithIO(p, bytes.NewReader(data), &out)
    os.Setenv("MOCHI_ROOT", root)
    if !bench {
        vm.SetNowSeed(1)
    }
    if err := m.Run(); err != nil {
        _ = os.WriteFile(errPath, []byte("run: "+err.Error()), 0o644)
        t.Fatalf("run: %v", err)
    }
    _ = os.Remove(errPath)

    got := bytes.TrimSpace(out.Bytes())
    if bench {
        benchPath := filepath.Join(outDir, "15.bench")
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
    outDir := filepath.Join(root, "tests", "spoj", "x", "mochi")
    md := filepath.Join(root, "transpiler", "x", "mochi", "SPOJ.md")
    status := " "
    dur := ""
    mem := ""
    if _, err := os.Stat(filepath.Join(outDir, "15.error")); err == nil {
        status = "error"
    } else if _, err := os.Stat(filepath.Join(outDir, "15.mochi")); err == nil {
        status = "✓"
    }
    if data, err := os.ReadFile(filepath.Join(outDir, "15.bench")); err == nil {
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
    buf.WriteString("Generated Mochi code from programs in `tests/spoj/x/mochi` lives in `tests/spoj/x/mochi`.\n")
    loc := time.FixedZone("GMT+7", 7*60*60)
    buf.WriteString("Last updated: " + time.Now().In(loc).Format("2006-01-02 15:04 MST") + "\n\n")
    buf.WriteString("## SPOJ Golden Test Checklist (" + fmt.Sprintf("%d", compiled) + "/1)\n")
    buf.WriteString("| Index | Name | Status | Duration | Memory |\n")
    buf.WriteString("|------:|------|:-----:|---------:|-------:|\n")
    buf.WriteString("| 15 | Add Two Numbers | " + status + " | " + dur + " | " + mem + " |\n")
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

