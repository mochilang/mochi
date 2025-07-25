//go:build slow

package scheme_test

import (
	"bufio"
	"bytes"
	"encoding/json"
	"flag"
	"fmt"
	"mochi/parser"
	scheme "mochi/transpiler/x/scheme"
	"mochi/types"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
	"time"
)

var update = flag.Bool("update-rosetta-scheme", false, "update golden files")

func updateEnabled() bool { return *update }

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

func TestSchemeTranspiler_Rosetta_Golden(t *testing.T) {
	if _, err := exec.LookPath("chibi-scheme"); err != nil {
		t.Skip("scheme not installed")
	}
	root := findRepoRoot2(t)
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "scheme")
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	os.MkdirAll(outDir, 0o755)
	t.Cleanup(updateRosettaChecklist)

	names, err := readIndex(filepath.Join(srcDir, "index.txt"))
	if err != nil {
		t.Fatalf("read index: %v", err)
	}
	if len(names) == 0 {
		t.Fatalf("no Mochi files found: %s", srcDir)
	}
	if idxStr := os.Getenv("MOCHI_ROSETTA_INDEX"); idxStr != "" {
		idx, err := strconv.Atoi(idxStr)
		if err != nil || idx < 1 || idx > len(names) {
			t.Fatalf("invalid MOCHI_ROSETTA_INDEX: %s", idxStr)
		}
		names = names[idx-1 : idx]
	} else if only := os.Getenv("MOCHI_ROSETTA_ONLY"); only != "" {
		names = []string{only + ".mochi"}
	}

	var firstFail string
	for _, nameFile := range names {
		src := filepath.Join(srcDir, nameFile)
		base := strings.TrimSuffix(nameFile, ".mochi")
		ok := t.Run(base, func(t *testing.T) {
			codePath := filepath.Join(outDir, base+".scm")
			outPath := filepath.Join(outDir, base+".out")
			errPath := filepath.Join(outDir, base+".error")

			want, err := os.ReadFile(outPath)
			if err != nil && !updateEnabled() {
				t.Fatalf("read want: %v", err)
			}
			want = bytes.TrimSpace(want)

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
			bench := os.Getenv("MOCHI_BENCHMARK") != ""
			scheme.SetBenchMain(bench)
			ast, err := scheme.Transpile(prog, env)
			if err != nil {
				_ = os.WriteFile(errPath, []byte(err.Error()), 0o644)
				t.Fatalf("transpile: %v", err)
			}
			code := scheme.Format(scheme.EmitString(ast))
			if err := os.WriteFile(codePath, code, 0o644); err != nil {
				t.Fatalf("write code: %v", err)
			}
			cmd := exec.Command("chibi-scheme", "-q", "-m", "chibi", "-m", "srfi.1", "-m", "srfi.69", "-m", "scheme.sort", "-m", "chibi.string", codePath)
			cmd.Env = append(os.Environ(), "MOCHI_NOW_SEED=1")
			if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
				cmd.Stdin = bytes.NewReader(data)
			}
			out, err := cmd.CombinedOutput()
			if err != nil {
				_ = os.WriteFile(errPath, append([]byte(err.Error()+"\n"), out...), 0o644)
				t.Fatalf("run: %v", err)
			}
			outBytes := bytes.TrimSpace(out)
			if bench {
				benchPath := filepath.Join(outDir, base+".bench")
				_ = os.WriteFile(benchPath, outBytes, 0o644)
				_ = os.Remove(errPath)
				var js struct {
					Duration int64  `json:"duration_us"`
					Memory   int64  `json:"memory_bytes"`
					Name     string `json:"name"`
				}
				_ = json.Unmarshal(outBytes, &js)
				return
			}
			_ = os.WriteFile(outPath, outBytes, 0o644)
			_ = os.Remove(errPath)
			if updateEnabled() || len(want) == 0 {
				return
			}
			if !bytes.Equal(outBytes, want) {
				t.Errorf("output mismatch for %s\n\n--- Got ---\n%s\n\n--- Want ---\n%s", base+".out", outBytes, want)
			}
		})
		if !ok {
			firstFail = base
			break
		}
	}
	if firstFail != "" {
		t.Fatalf("first failing program: %s", firstFail)
	}
}

func updateRosettaChecklist() {
	root := findRepoRoot2(&testing.T{})
	srcDir := filepath.Join(root, "tests", "rosetta", "x", "Mochi")
	outDir := filepath.Join(root, "tests", "rosetta", "transpiler", "scheme")
	readmePath := filepath.Join(root, "transpiler", "x", "scheme", "ROSETTA.md")

	names, err := readIndex(filepath.Join(srcDir, "index.txt"))
	if err != nil {
		return
	}
	total := len(names)
	compiled := 0
	var lines []string
	lines = append(lines, "| Index | Name | Status | Duration | Memory |")
	lines = append(lines, "|------:|------|:-----:|---------:|-------:|")
	for i, nameFile := range names {
		name := strings.TrimSuffix(nameFile, ".mochi")
		status := " "
		if _, err := os.Stat(filepath.Join(outDir, name+".error")); err == nil {
			// leave unchecked
		} else if _, err := os.Stat(filepath.Join(outDir, name+".scm")); err == nil {
			compiled++
			status = "✓"
		}
		dur := ""
		mem := ""
		if data, err := os.ReadFile(filepath.Join(outDir, name+".bench")); err == nil {
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
	ts := time.Now().UTC().Format("2006-01-02 15:04 MST")
	var buf bytes.Buffer
	buf.WriteString("# Scheme Rosetta Transpiler Output\n\n")
	buf.WriteString("Generated Scheme code for Rosetta Code tasks under `tests/rosetta/x/Mochi`.\n\n")
	fmt.Fprintf(&buf, "## Checklist (%d/%d)\n", compiled, total)
	buf.WriteString("Last updated: " + ts + "\n\n")
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
