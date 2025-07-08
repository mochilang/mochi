//go:build slow

package rubycode_test

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	rubycode "mochi/compiler/x/ruby"
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

func TestRubyCompiler_ValidPrograms(t *testing.T) {
	root := findRoot(t)
	pattern := filepath.Join(root, "tests", "vm", "valid", "*.mochi")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatalf("glob error: %v", err)
	}
	outDir := filepath.Join(root, "tests", "machine", "x", "ruby")
	os.MkdirAll(outDir, 0755)
	compiled := 0
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		t.Run(name, func(t *testing.T) {
			prog, err := parser.Parse(src)
			if err != nil {
				t.Fatalf("parse error: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				t.Fatalf("type error: %v", errs[0])
			}
			c := rubycode.New(env)
			code, err := c.Compile(prog)
			if err != nil {
				errFile := filepath.Join(outDir, name+".error")
				os.WriteFile(errFile, []byte(err.Error()), 0644)
				t.Fatalf("compile error: %v", err)
			}
			rbFile := filepath.Join(outDir, name+".rb")
			if err := os.WriteFile(rbFile, code, 0644); err != nil {
				t.Fatalf("write error: %v", err)
			}
			cmd := exec.Command("ruby", rbFile)
			if data, err := os.ReadFile(strings.TrimSuffix(src, ".mochi") + ".in"); err == nil {
				cmd.Stdin = bytes.NewReader(data)
			}
			out, err := cmd.CombinedOutput()
			if err != nil {
				line := 0
				parts := strings.Split(string(out), "\n")
				for _, p := range parts {
					if strings.HasPrefix(p, rbFile+":") {
						fmt.Sscanf(p, rbFile+":%d", &line)
						break
					}
				}
				ctx := []string{}
				if line > 0 {
					data, _ := os.ReadFile(rbFile)
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
				t.Fatalf("ruby error: %v\n%s", err, out)
			}
			norm := normalize(out)
			os.WriteFile(filepath.Join(outDir, name+".out"), norm, 0644)
			compiled++
		})
	}

	// update README with results
	readme := filepath.Join(outDir, "README.md")
	entries := []string{}
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		if _, err := os.Stat(filepath.Join(outDir, name+".out")); err == nil {
			entries = append(entries, "- [x] "+name+".mochi")
		} else {
			entries = append(entries, "- [ ] "+name+".mochi")
		}
	}
	content := fmt.Sprintf("# Ruby Compiler Results\n\nCompiled programs: %d/97\n\n%s\n", compiled, strings.Join(entries, "\n"))
	os.WriteFile(readme, []byte(content), 0644)
}

func normalize(out []byte) []byte {
	lines := strings.Split(strings.TrimSpace(string(out)), "\n")
	for i, ln := range lines {
		if ln == "true" || ln == "false" || ln == "nil" {
			// keep
		}
		if strings.HasPrefix(ln, "[") && strings.HasSuffix(ln, "]") {
			ln = strings.TrimSuffix(strings.TrimPrefix(ln, "["), "]")
			ln = strings.ReplaceAll(ln, ", ", " ")
		}
		lines[i] = ln
	}
	return []byte(strings.Join(lines, "\n"))
}
