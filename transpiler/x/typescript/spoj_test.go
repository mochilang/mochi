//go:build slow

package typescript_test

import (
    "bytes"
    "encoding/json"
    "os"
    "os/exec"
    "path/filepath"
    "strconv"
    "testing"
    "time"

    "mochi/parser"
    tstranspiler "mochi/transpiler/x/ts"
    "mochi/types"
)

var indexes = []int{1,2,3,4,5,6,7,8,10,11}

var names = map[int]string{
    1:"Life, the Universe, and Everything",
    2:"PRIME1 - Prime Generator",
    3:"Substring Check (Bug Funny)",
    4:"SPOJ ONP - Transform the Expression",
    5:"SPOJ Problem 5: The Next Palindrome",
    6:"ARITH - Simple Arithmetics",
    7:"BULK - The Bulk!",
    8:"CMPLS - Complete the Sequence!",
    10:"CMEXPR - Complicated Expressions",
    11:"FCTRL - Factorial",
}

func TestTypescriptTranspiler_SPOJ_Golden(t *testing.T) {
    if _, err := exec.LookPath("deno"); err != nil {
        t.Skip("deno not installed")
    }
    root := repoRoot(t)
    srcDir := filepath.Join(root, "tests", "spoj", "human", "x", "mochi")
    outDir := filepath.Join(root, "tests", "spoj", "x", "typescript")
    os.MkdirAll(outDir, 0o755)
    t.Cleanup(updateSPOJ)

    bench := os.Getenv("MOCHI_BENCHMARK") != ""

    for _, idx := range indexes {
        base := strconv.Itoa(idx)
        t.Run(base, func(t *testing.T) {
            src := filepath.Join(srcDir, base+".mochi")
            codePath := filepath.Join(outDir, base+".ts")
            wantPath := filepath.Join(outDir, base+".out")
            errPath := filepath.Join(outDir, base+".error")
            benchPath := filepath.Join(outDir, base+".bench")

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
            inPath := filepath.Join(outDir, base+".in")
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
                idx := bytes.LastIndexByte(got, '{')
                if idx >= 0 {
                    _ = os.WriteFile(benchPath, got[idx:], 0o644)
                } else {
                    _ = os.WriteFile(benchPath, got, 0o644)
                }
                return
            }

            want, err := os.ReadFile(wantPath)
            if err != nil {
                t.Fatalf("read want: %v", err)
            }
            want = bytes.TrimSpace(want)
            if !bytes.Equal(got, want) {
                t.Fatalf("output mismatch\n\n--- Got ---\n%s\n\n--- Want ---\n%s", got, want)
            }
            _ = os.Remove(benchPath)
        })
    }
}

func updateSPOJ() {
    root := repoRoot(&testing.T{})
    outDir := filepath.Join(root, "tests", "spoj", "x", "typescript")
    md := filepath.Join(root, "transpiler", "x", "typescript", "SPOJ.md")
    var buf bytes.Buffer
    buf.WriteString("# SPOJ Transpiler Progress\n\n")
    buf.WriteString("This checklist is auto-generated.\n")
    buf.WriteString("Generated TypeScript code from programs in `tests/spoj/human/x/mochi` lives in `tests/spoj/x/typescript`.\n")
    loc := time.FixedZone("GMT+7", 7*60*60)
    buf.WriteString("Last updated: " + time.Now().In(loc).Format("2006-01-02 15:04 MST") + "\n\n")
    buf.WriteString("## SPOJ Golden Test Checklist (" + strconv.Itoa(len(indexes)) + "/" + strconv.Itoa(len(indexes)) + ")\n")
    buf.WriteString("| Index | Name | Status | Duration | Memory |\n")
    buf.WriteString("|------:|------|:-----:|---------:|-------:|\n")
    for _, idx := range indexes {
        base := strconv.Itoa(idx)
        status := " "
        dur := ""
        mem := ""
        if _, err := os.Stat(filepath.Join(outDir, base+".error")); err == nil {
            status = "error"
        } else if _, err := os.Stat(filepath.Join(outDir, base+".ts")); err == nil {
            status = "✓"
        }
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
        name := names[idx]
        buf.WriteString("| " + base + " | " + name + " | " + status + " | " + dur + " | " + mem + " |\n")
    }
    _ = os.WriteFile(md, buf.Bytes(), 0o644)
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

func formatDuration(d time.Duration) string {
    switch {
    case d < time.Microsecond:
        return strconv.FormatInt(d.Nanoseconds(), 10) + "ns"
    case d < time.Millisecond:
        return strconv.FormatFloat(float64(d.Microseconds()), 'f', 1, 64) + "µs"
    case d < time.Second:
        return strconv.FormatFloat(float64(d.Milliseconds()), 'f', 1, 64) + "ms"
    default:
        return strconv.FormatFloat(d.Seconds(), 'f', 2, 64) + "s"
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
        return strconv.FormatFloat(float64(n)/float64(_GB), 'f', 2, 64) + "GB"
    case n >= _MB:
        return strconv.FormatFloat(float64(n)/float64(_MB), 'f', 2, 64) + "MB"
    case n >= _KB:
        return strconv.FormatFloat(float64(n)/float64(_KB), 'f', 2, 64) + "KB"
    default:
        return strconv.FormatInt(n, 10) + "B"
    }
}
