//go:build slow

package ex_test

import (
        "bufio"
        "bytes"
        "encoding/json"
        "flag"
        "fmt"
        "os"
        "os/exec"
        "path/filepath"
        "strconv"
        "strings"
        "testing"
        "time"

        "mochi/parser"
        ex "mochi/transpiler/x/ex"
        "mochi/types"
)

var updateAlgorithmsFlag = flag.Bool("update-algorithms-ex", false, "update golden files")

func algUpdateEnabled() bool { return *updateAlgorithmsFlag }

func runAlgorithmCase(t *testing.T, file string) {
        t.Helper()
        ensureElixir(t)
        root := repoRoot(t)
        src := filepath.Join(root, "tests", "github", "TheAlgorithms", "Mochi", file)
        outDir := filepath.Join(root, "tests", "algorithms", "x", "Elixir", filepath.Dir(file))
        os.MkdirAll(outDir, 0o755)
        base := strings.TrimSuffix(filepath.Base(file), ".mochi")
        codePath := filepath.Join(outDir, base+".exs")
        outPath := filepath.Join(outDir, base+".out")
        errPath := filepath.Join(outDir, base+".error")

        bench := os.Getenv("MOCHI_BENCHMARK") != ""
        want, err := os.ReadFile(outPath)
        if err != nil {
                if !algUpdateEnabled() {
                        t.Fatalf("read want: %v", err)
                }
        } else if !bench {
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
        ast, err := ex.Transpile(prog, env)
        if err != nil {
                _ = os.WriteFile(errPath, []byte("transpile: "+err.Error()), 0o644)
                t.Fatalf("transpile: %v", err)
        }
        code := ex.Emit(ast, bench)
        if err := os.WriteFile(codePath, code, 0o644); err != nil {
                t.Fatalf("write code: %v", err)
        }
        cmd := exec.Command("elixir", codePath)
        envv := os.Environ()
        if !bench {
                envv = append(envv, "MOCHI_NOW_SEED=1")
        } else {
                envv = append(envv, "MOCHI_BENCHMARK=true")
        }
        cmd.Env = envv
        if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
                cmd.Stdin = bytes.NewReader(data)
        }
        out, err := cmd.CombinedOutput()
        got := bytes.TrimSpace(out)
        if err != nil {
                if algUpdateEnabled() {
                        _ = os.WriteFile(errPath, append([]byte("run: "+err.Error()+"\n"), out...), 0o644)
                } else {
                        _ = os.WriteFile(errPath, append([]byte("run: "+err.Error()+"\n"), out...), 0o644)
                        t.Fatalf("run: %v", err)
                }
        } else {
                _ = os.Remove(errPath)
        }

        if bench {
                benchPath := filepath.Join(outDir, base+".bench")
                if algUpdateEnabled() {
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
                }
                return
        }

        if algUpdateEnabled() {
                _ = os.WriteFile(outPath, got, 0o644)
                return
        }

        if !bytes.Equal(got, want) {
                t.Errorf("output mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s", file, got, want)
        }
}

func TestExTranspiler_Algorithms_Golden(t *testing.T) {
        root := repoRoot(t)
        t.Cleanup(updateAlgorithms)
        indexPath := filepath.Join(root, "tests", "github", "TheAlgorithms", "Mochi", "index.txt")
        f, err := os.Open(indexPath)
        if err != nil {
                t.Fatalf("open index: %v", err)
        }
        defer f.Close()
        scanner := bufio.NewScanner(f)
        type entry struct {
                idx  int
                file string
        }
        var entries []entry
        for scanner.Scan() {
                parts := strings.Fields(scanner.Text())
                if len(parts) != 2 {
                        continue
                }
                n, err := strconv.Atoi(parts[0])
                if err != nil {
                        continue
                }
                entries = append(entries, entry{idx: n, file: parts[1]})
        }
        if err := scanner.Err(); err != nil {
                t.Fatalf("read index: %v", err)
        }
        if len(entries) == 0 {
                t.Fatal("empty index")
        }
        if v := os.Getenv("MOCHI_ALG_INDEX"); v != "" {
                if n, err := strconv.Atoi(v); err == nil && n >= 1 && n <= len(entries) {
                        entries = entries[n-1 : n]
                } else {
                        t.Fatalf("invalid MOCHI_ALG_INDEX: %s", v)
                }
        }
        max := len(entries)
        if v := os.Getenv("ALGORITHMS_MAX"); v != "" {
                if n, err := strconv.Atoi(v); err == nil && n < max {
                        max = n
                }
        }
        for _, e := range entries[:max] {
                idxName := fmt.Sprintf("%03d_%s", e.idx, strings.TrimSuffix(filepath.Base(e.file), ".mochi"))
                if ok := t.Run(idxName, func(t *testing.T) { runAlgorithmCase(t, e.file) }); !ok {
                        t.Fatalf("first failing program: %s", e.file)
                }
        }
}

func updateAlgorithms() {
        root := repoRoot(&testing.T{})
        srcDir := filepath.Join(root, "tests", "github", "TheAlgorithms", "Mochi")
        outDir := filepath.Join(root, "tests", "algorithms", "x", "Elixir")
        md := filepath.Join(root, "transpiler", "x", "ex", "ALGORITHMS.md")
        indexPath := filepath.Join(srcDir, "index.txt")
        f, err := os.Open(indexPath)
        if err != nil {
                return
        }
        defer f.Close()
        scanner := bufio.NewScanner(f)
        type entry struct {
                idx  int
                file string
        }
        var entries []entry
        for scanner.Scan() {
                parts := strings.Fields(scanner.Text())
                if len(parts) != 2 {
                        continue
                }
                n, err := strconv.Atoi(parts[0])
                if err != nil {
                        continue
                }
                entries = append(entries, entry{idx: n, file: parts[1]})
        }
        total := len(entries)
        compiled := 0
        var rows []string
        rows = append(rows, "| Index | Name | Status | Duration | Memory |")
        rows = append(rows, "|------:|------|:-----:|---------:|-------:|")
        for _, e := range entries {
                name := strings.TrimSuffix(e.file, ".mochi")
                status := " "
                dur := ""
                mem := ""
                if _, err := os.Stat(filepath.Join(outDir, name+".error")); err == nil {
                        status = "error"
                } else if _, err := os.Stat(filepath.Join(outDir, name+".exs")); err == nil {
                        status = "✓"
                        compiled++
                }
                if data, err := os.ReadFile(filepath.Join(outDir, name+".bench")); err == nil {
                        var r struct {
                                DurationUS  int64 `json:"duration_us"`
                                MemoryBytes int64 `json:"memory_bytes"`
                        }
                        if json.Unmarshal(bytes.TrimSpace(data), &r) == nil {
                                dur = formatDuration(time.Duration(r.DurationUS) * time.Microsecond)
                                mem = formatBytes(r.MemoryBytes)
                        }
                }
                rows = append(rows, fmt.Sprintf("| %d | %s | %s | %s | %s |", e.idx, name, status, dur, mem))
        }
        var buf bytes.Buffer
        buf.WriteString("# Algorithms Transpiler Progress\n\n")
        buf.WriteString("This checklist is auto-generated.\n")
        buf.WriteString("Generated Elixir code from programs in `tests/github/TheAlgorithms/Mochi` lives in `tests/algorithms/x/Elixir`.\n")
        loc := time.FixedZone("GMT+7", 7*60*60)
        buf.WriteString("Last updated: " + time.Now().In(loc).Format("2006-01-02 15:04 MST") + "\n\n")
        fmt.Fprintf(&buf, "## Algorithms Golden Test Checklist (%d/%d)\n", compiled, total)
        buf.WriteString(strings.Join(rows, "\n"))
        buf.WriteString("\n")
        _ = os.WriteFile(md, buf.Bytes(), 0o644)
}

func formatDuration(d time.Duration) string {
        if d < time.Millisecond {
                return fmt.Sprintf("%dus", d.Microseconds())
        }
        if d < time.Second {
                return fmt.Sprintf("%.2fms", float64(d.Microseconds())/1000)
        }
        if d < time.Minute {
                return fmt.Sprintf("%.2fs", d.Seconds())
        }
        return d.String()
}

func formatBytes(n int64) string {
        const unit = 1024
        if n < unit {
                return fmt.Sprintf("%d B", n)
        }
        val := float64(n)
        exp := 0
        units := []string{"KB", "MB", "GB", "TB", "PB", "EB"}
        for val >= unit && exp < len(units)-1 {
                val /= unit
                exp++
        }
        return fmt.Sprintf("%.1f %s", val, units[exp])
}
