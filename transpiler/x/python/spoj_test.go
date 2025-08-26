//go:build slow

package python_test

import (
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
        py "mochi/transpiler/x/py"
        "mochi/types"
)

func ensurePython(t *testing.T) {
	if _, err := exec.LookPath("python3"); err != nil {
		t.Skip("python3 not installed")
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

func TestPythonTranspiler_SPOJ_Golden(t *testing.T) {
        ensurePython(t)
        root := repoRoot(t)
        t.Cleanup(updateSPOJ)

        outDir := filepath.Join(root, "tests", "spoj", "x", "python")
        os.MkdirAll(outDir, 0o755)
        bench := os.Getenv("MOCHI_BENCHMARK") != ""

        for i := 1; i <= 5; i++ {
                idx := strconv.Itoa(i)
                src := filepath.Join(root, "tests", "spoj", "x", "mochi", idx+".mochi")
                codePath := filepath.Join(outDir, idx+".py")
                outPath := filepath.Join(outDir, idx+".out")
                errPath := filepath.Join(outDir, idx+".error")

                want, err := os.ReadFile(outPath)
                if err != nil {
                        t.Fatalf("read want %s: %v", idx, err)
                }
                if !bench {
                        want = bytes.TrimSpace(want)
                }

                prog, err := parser.Parse(src)
                if err != nil {
                        _ = os.WriteFile(errPath, []byte("parse: "+err.Error()), 0o644)
                        t.Fatalf("parse %s: %v", idx, err)
                }
                env := types.NewEnv(nil)
                if errs := types.Check(prog, env); len(errs) > 0 {
                        _ = os.WriteFile(errPath, []byte("type: "+errs[0].Error()), 0o644)
                        t.Fatalf("type %s: %v", idx, errs[0])
                }
                ast, err := py.Transpile(prog, env, bench)
                if err != nil {
                        _ = os.WriteFile(errPath, []byte("transpile: "+err.Error()), 0o644)
                        t.Fatalf("transpile %s: %v", idx, err)
                }
                var buf bytes.Buffer
                if err := py.Emit(&buf, ast, bench); err != nil {
                        _ = os.WriteFile(errPath, []byte("emit: "+err.Error()), 0o644)
                        t.Fatalf("emit %s: %v", idx, err)
                }
                if err := os.WriteFile(codePath, buf.Bytes(), 0o644); err != nil {
                        t.Fatalf("write code %s: %v", idx, err)
                }

                cmd := exec.Command("python3", codePath)
                envv := os.Environ()
                if !bench {
                        envv = append(envv, "MOCHI_NOW_SEED=1")
                }
                cmd.Env = envv
                inPath := filepath.Join(outDir, idx+".in")
                if data, err := os.ReadFile(inPath); err == nil {
                        cmd.Stdin = bytes.NewReader(data)
                }
                out, err := cmd.CombinedOutput()
                got := bytes.TrimSpace(out)
                if err != nil {
                        _ = os.WriteFile(errPath, append([]byte("run: "+err.Error()+"\n"), out...), 0o644)
                        t.Fatalf("run %s: %v", idx, err)
                }
                _ = os.Remove(errPath)

                if bench {
                        benchPath := filepath.Join(outDir, idx+".bench")
                        j := bytes.LastIndex(got, []byte("{"))
                        if j >= 0 {
                                part := bytes.TrimSpace(got[j:])
                                if json.Valid(part) {
                                        _ = os.WriteFile(benchPath, part, 0o644)
                                } else {
                                        _ = os.WriteFile(benchPath, got, 0o644)
                                }
                        } else {
                                _ = os.WriteFile(benchPath, got, 0o644)
                        }
                        continue
                }

                if !bytes.Equal(got, want) {
                        t.Fatalf("%s: output mismatch\n\n--- Got ---\n%s\n\n--- Want ---\n%s", idx, got, want)
                }
        }
}

func updateSPOJ() {
        root := repoRoot(&testing.T{})
        outDir := filepath.Join(root, "tests", "spoj", "x", "python")
        md := filepath.Join(root, "transpiler", "x", "python", "SPOJ.md")

        type row struct {
                idx int
                name string
                status string
                dur string
                mem string
        }

        names := map[int]string{}
        for i := 1; i <= 5; i++ {
                path := filepath.Join(root, "tests", "spoj", "x", "mochi", fmt.Sprintf("%d.md", i))
                if data, err := os.ReadFile(path); err == nil {
                        line := strings.TrimSpace(strings.SplitN(string(data), "\n", 2)[0])
                        line = strings.TrimPrefix(line, "#")
                        line = strings.TrimSpace(line)
                        if strings.HasPrefix(line, "[") {
                                if r := strings.Index(line, "]"); r > 1 {
                                        names[i] = line[1:r]
                                }
                        }
                        if names[i] == "" {
                                names[i] = line
                        }
                } else {
                        names[i] = fmt.Sprintf("Problem %d", i)
                }
        }

        var rows []row
        compiled := 0
        for i := 1; i <= 5; i++ {
                r := row{idx: i, name: names[i], status: " "}
                if _, err := os.Stat(filepath.Join(outDir, fmt.Sprintf("%d.error", i))); err == nil {
                        r.status = "error"
                } else if _, err := os.Stat(filepath.Join(outDir, fmt.Sprintf("%d.py", i))); err == nil {
                        r.status = "✓"
                        compiled++
                }
                if data, err := os.ReadFile(filepath.Join(outDir, fmt.Sprintf("%d.bench", i))); err == nil {
                        var b struct {
                                DurationUS  int64 `json:"duration_us"`
                                MemoryBytes int64 `json:"memory_bytes"`
                        }
                        if json.Unmarshal(bytes.TrimSpace(data), &b) == nil {
                                r.dur = formatDuration(time.Duration(b.DurationUS) * time.Microsecond)
                                r.mem = formatBytes(b.MemoryBytes)
                        }
                }
                rows = append(rows, r)
        }

        var buf bytes.Buffer
        buf.WriteString("# SPOJ Transpiler Progress\n\n")
        buf.WriteString("This checklist is auto-generated.\n")
        buf.WriteString("Generated Python code from programs in `tests/spoj/x/mochi` lives in `tests/spoj/x/python`.\n")
        loc := time.FixedZone("GMT+7", 7*60*60)
        buf.WriteString("Last updated: " + time.Now().In(loc).Format("2006-01-02 15:04 MST") + "\n\n")
        buf.WriteString("## SPOJ Golden Test Checklist (" + strconv.Itoa(compiled) + "/5)\n")
        buf.WriteString("| Index | Name | Status | Duration | Memory |\n")
        buf.WriteString("|------:|------|:-----:|---------:|-------:|\n")
        for _, r := range rows {
                buf.WriteString(fmt.Sprintf("| %d | %s | %s | %s | %s |\n", r.idx, r.name, r.status, r.dur, r.mem))
        }
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
