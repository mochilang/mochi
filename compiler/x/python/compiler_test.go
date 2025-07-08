//go:build slow

package pycode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	pycode "mochi/compiler/x/python"
	"mochi/parser"
	"mochi/types"
)

func findRoot(t *testing.T) string {
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

func TestPythonCompiler_ValidPrograms(t *testing.T) {
	root := findRoot(t)
	pattern := filepath.Join(root, "tests", "vm", "valid", "*.mochi")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatalf("glob error: %v", err)
	}
	outDir := filepath.Join(root, "tests", "machine", "x", "python")
	os.MkdirAll(outDir, 0755)
       for _, src := range files {
               name := strings.TrimSuffix(filepath.Base(src), ".mochi")
               data, err := os.ReadFile(src)
               if err != nil {
                       t.Fatalf("read error: %v", err)
               }
               t.Run(name, func(t *testing.T) {
                       if name == "load_yaml" {
				if err := exec.Command("python3", "-c", "import yaml").Run(); err != nil {
					t.Skip("yaml module not installed")
				}
			}
                       prog, err := parser.Parse(src)
                       if err != nil {
                               writeError(outDir, name, string(data), err)
                               t.Fatalf("parse error: %v", err)
                       }
                       env := types.NewEnv(nil)
                       if errs := types.Check(prog, env); len(errs) > 0 {
                               writeError(outDir, name, string(data), errs[0])
                               t.Fatalf("type error: %v", errs[0])
                       }
                       c := pycode.New(env)
                       code, err := c.Compile(prog)
                       if err != nil {
                               writeError(outDir, name, string(data), err)
                               t.Fatalf("compile error: %v", err)
                       }
                       pyFile := filepath.Join(outDir, name+".py")
                       if err := os.WriteFile(pyFile, code, 0644); err != nil {
                               t.Fatalf("write error: %v", err)
                       }
			cmd := exec.Command("python3", pyFile)
			if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
				cmd.Stdin = bytes.NewReader(data)
			}
			out, err := cmd.CombinedOutput()
			if err != nil {
				// parse line number if available
				line := 0
				parts := strings.Split(string(out), "\n")
				for i := len(parts) - 1; i >= 0; i-- {
					if strings.HasPrefix(parts[i], "  File") && strings.Contains(parts[i], pyFile) {
						fmt.Sscanf(parts[i], "  File %q, line %d", new(string), &line)
						break
					}
				}
				ctx := []string{}
				if line > 0 {
					data, _ := os.ReadFile(pyFile)
					lines := strings.Split(string(data), "\n")
					start := line - 2
					if start < 0 {
						start = 0
					}
					end := line + 1
					if end > len(lines) {
						end = len(lines)
					}
					for i := start; i < end; i++ {
						ctx = append(ctx, lines[i])
					}
				}
				errFile := filepath.Join(outDir, name+".error")
				msg := fmt.Sprintf("line %d: %v\n%s", line, err, string(out))
				if len(ctx) > 0 {
					msg += "\n" + strings.Join(ctx, "\n")
				}
				os.WriteFile(errFile, []byte(msg), 0644)
				t.Fatalf("python error: %v\n%s", err, out)
			}
			norm := normalize(out)
			os.WriteFile(filepath.Join(outDir, name+".out"), norm, 0644)
		})
	}
}

func normalize(out []byte) []byte {
	lines := strings.Split(strings.TrimSpace(string(out)), "\n")
	for i, ln := range lines {
		if ln == "True" {
			ln = "true"
		} else if ln == "False" {
			ln = "false"
		}
		if strings.HasPrefix(ln, "[") && strings.HasSuffix(ln, "]") {
			ln = strings.TrimSuffix(strings.TrimPrefix(ln, "["), "]")
			ln = strings.ReplaceAll(ln, ", ", " ")
			ln = strings.ReplaceAll(ln, "'", "")
		}
		lines[i] = ln
	}
        return []byte(strings.Join(lines, "\n"))
}

// writeError writes an error file with context around the failing line.
func writeError(dir, name, src string, err error) {
       lines := strings.Split(src, "\n")
       ln := 0
       if idx := strings.Index(err.Error(), "line "); idx != -1 {
               fmt.Sscanf(err.Error()[idx:], "line %d", &ln)
       }
       var ctx string
       if ln > 0 {
               start := ln - 2
               if start < 0 {
                       start = 0
               }
               end := ln + 1
               if end > len(lines) {
                       end = len(lines)
               }
               ctx = strings.Join(lines[start:end], "\n")
       }
       msg := fmt.Sprintf("line %d: %v", ln, err)
       if ctx != "" {
               msg += "\n" + ctx
       }
       os.WriteFile(filepath.Join(dir, name+".error"), []byte(msg+"\n"), 0644)
}
