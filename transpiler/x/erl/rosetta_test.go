//go:build rosetta

package erl_test

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
	erl "mochi/transpiler/x/erl"
	"mochi/types"
)

var update = flag.Bool("update", false, "update golden files")

func repoRoot(t *testing.T) string {
	dir, err := os.Getwd()
	if err != nil {
		t.Fatal(err)
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

func updateEnabled() bool { return *update }

func TestRosettaTranspile(t *testing.T) {
	if _, err := exec.LookPath("escript"); err != nil {
		t.Skip("escript not installed")
	}
       root := repoRoot(t)
       srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
       outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "erl")
       os.MkdirAll(outDir, 0o755)

       names, err := readIndex(filepath.Join(srcDir, "index.txt"))
       if err != nil {
               t.Fatalf("read index: %v", err)
       }
       if len(names) == 0 {
               t.Fatal("no mochi files found")
       }

       start := 0
       if idxStr := os.Getenv("MOCHI_ROSETTA_INDEX"); idxStr != "" {
               idx, err := strconv.Atoi(idxStr)
               if err != nil || idx < 1 || idx > len(names) {
                       t.Fatalf("invalid MOCHI_ROSETTA_INDEX: %s", idxStr)
               }
               start = idx - 1
               names = names[start : start+1]
       } else {
               max := len(names)
               if v := os.Getenv("ROSETTA_MAX"); v != "" {
                       if n, err := strconv.Atoi(v); err == nil && n < max {
                               max = n
                       }
               }
               names = names[:max]
       }

       var firstFail string
       for i, nameFile := range names {
               name := strings.TrimSuffix(nameFile, ".mochi")
               idx := start + i + 1
               src := filepath.Join(srcDir, nameFile)
               ok := t.Run(fmt.Sprintf("%03d_%s", idx, name), func(t *testing.T) { runRosetta(t, src, name, outDir) })
               if !ok && firstFail == "" {
                       firstFail = name
                       break
               }
       }
	if firstFail != "" {
		t.Fatalf("first failing program: %s", firstFail)
	}
}

func runRosetta(t *testing.T, srcPath, name, outDir string) {
	erlFile := filepath.Join(outDir, name+".erl")
	outFile := filepath.Join(outDir, name+".out")
	errFile := filepath.Join(outDir, name+".error")

       prog, err := parser.Parse(srcPath)
       if err != nil {
               _ = os.WriteFile(errFile, []byte(fmt.Sprintf("parse error: %v", err)), 0o644)
               t.Fatalf("parse: %v", err)
       }
       env := types.NewEnv(nil)
       if errs := types.Check(prog, env); len(errs) > 0 {
               _ = os.WriteFile(errFile, []byte(fmt.Sprintf("type error: %v", errs[0])), 0o644)
               t.Fatalf("type: %v", errs[0])
       }
       bench := os.Getenv("MOCHI_BENCHMARK") == "true" || os.Getenv("MOCHI_BENCHMARK") == "1"
       erl.SetBenchMain(bench)
       ast, err := erl.Transpile(prog, env)
	if err != nil {
		_ = os.WriteFile(errFile, []byte(fmt.Sprintf("transpile error: %v", err)), 0o644)
		t.Fatalf("transpile: %v", err)
	}
	code := ast.Emit()
	if updateEnabled() {
		if err := os.WriteFile(erlFile, code, 0o644); err != nil {
			t.Fatalf("write erl: %v", err)
		}
	} else {
		_ = os.WriteFile(erlFile, code, 0o644)
	}

       cmd := exec.Command("escript", erlFile)
       envVars := append(os.Environ(), "MOCHI_NOW_SEED=1")
       if bench {
               envVars = append(envVars, "MOCHI_BENCHMARK=1")
       }
       cmd.Env = envVars
	if data, err := os.ReadFile(strings.TrimSuffix(srcPath, ".mochi") + ".in"); err == nil {
		cmd.Stdin = bytes.NewReader(data)
	}
	var buf bytes.Buffer
	cmd.Stdout = &buf
	cmd.Stderr = &buf
	if err := cmd.Run(); err != nil {
		if updateEnabled() {
			_ = os.WriteFile(errFile, append([]byte(err.Error()+"\n"), buf.Bytes()...), 0o644)
		}
		t.Fatalf("run: %v", err)
	}
       got := bytes.TrimSpace(buf.Bytes())
       if updateEnabled() {
               _ = os.WriteFile(outFile, got, 0o644)
               _ = os.Remove(errFile)
               return
       }
       if bench {
               var js struct {
                       Duration int64  `json:"duration_us"`
                       Memory   int64  `json:"memory_bytes"`
                       Name     string `json:"name"`
               }
               _ = json.Unmarshal(got, &js)
       }
       want, err := os.ReadFile(outFile)
	if err != nil {
		t.Fatalf("read golden: %v", err)
	}
	want = bytes.TrimSpace(want)
	if !bytes.Equal(got, want) {
		t.Errorf("output mismatch:\nGot: %s\nWant: %s", got, want)
	}
	_ = os.Remove(errFile)
}

func countRosetta() (int, int) {
        root := repoRoot(&testing.T{})
        srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
        outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "erl")
       names, err := readIndex(filepath.Join(srcDir, "index.txt"))
       if err != nil {
               return 0, 0
       }
       total := len(names)
       compiled := 0
       for _, nf := range names {
               name := strings.TrimSuffix(nf, ".mochi")
               if _, err := os.Stat(filepath.Join(outDir, name+".out")); err == nil {
                       compiled++
               }
       }
       return compiled, total
}

func updateRosettaReadme() {
	root := repoRoot(&testing.T{})
       srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
       outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "erl")
       readmePath := filepath.Join(root, "transpiler", "x", "erl", "ROSETTA.md")
       names, err := readIndex(filepath.Join(srcDir, "index.txt"))
       if err != nil {
               return
       }
       total := len(names)
       compiled := 0
       var lines []string
       lines = append(lines, "| Index | Name | Status | Duration | Memory |")
       lines = append(lines, "|------:|------|:-----:|---------:|-------:|")
       for i, nf := range names {
               name := strings.TrimSuffix(nf, ".mochi")
               status := " "
               if _, err := os.Stat(filepath.Join(outDir, name+".error")); err == nil {
                       // leave unchecked
               } else if _, err := os.Stat(filepath.Join(outDir, name+".erl")); err == nil {
                       compiled++
                       status = "✓"
               }
               dur := ""
               mem := ""
               if data, err := os.ReadFile(filepath.Join(outDir, name+".out")); err == nil {
                       var js struct {
                               Duration int64 `json:"duration_us"`
                               Memory   int64 `json:"memory_bytes"`
                       }
                       data = bytes.TrimSpace(data)
                       if idx := bytes.LastIndexByte(data, '{'); idx >= 0 {
                               if json.Unmarshal(data[idx:], &js) == nil && js.Duration > 0 {
                                       dur = humanDuration(js.Duration)
                                       mem = humanSize(js.Memory)
                               }
                       }
               }
               lines = append(lines, fmt.Sprintf("| %d | %s | %s | %s | %s |", i+1, name, status, dur, mem))
       }
	tsRaw, _ := exec.Command("git", "log", "-1", "--format=%cI").Output()
	ts := strings.TrimSpace(string(tsRaw))
	if t, err := time.Parse(time.RFC3339, ts); err == nil {
		if loc, lerr := time.LoadLocation("Asia/Bangkok"); lerr == nil {
			ts = t.In(loc).Format("2006-01-02 15:04 -0700")
		} else {
			ts = t.Format("2006-01-02 15:04 MST")
		}
	}
	var buf bytes.Buffer
	fmt.Fprintf(&buf, "# Erlang Rosetta Output (%d/%d compiled and run)\n", compiled, total)
	fmt.Fprintf(&buf, "Last updated: %s\n\n", ts)
	buf.WriteString("This directory contains Erlang code generated by the Mochi transpiler for Rosetta Code tasks. Each program in `tests/rosetta/x/Mochi` is transpiled and executed with `escript`.\n\n")
	buf.WriteString("## Program checklist\n\n")
       buf.WriteString(strings.Join(lines, "\n"))
       buf.WriteString("\n")
       _ = os.WriteFile(readmePath, buf.Bytes(), 0o644)
}

func humanDuration(us int64) string {
       d := time.Duration(us) * time.Microsecond
       return d.String()
}

func humanSize(b int64) string {
       const unit = 1024
       if b < unit {
               return fmt.Sprintf("%d B", b)
       }
       div, exp := int64(unit), 0
       for n := b / unit; n >= unit; n /= unit {
               div *= unit
               exp++
       }
       return fmt.Sprintf("%.1f %cB", float64(b)/float64(div), "KMGTPE"[exp])
}

func readIndex(path string) ([]string, error) {
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

func TestMain(m *testing.M) {
	code := m.Run()
	updateRosettaReadme()
	os.Exit(code)
}
