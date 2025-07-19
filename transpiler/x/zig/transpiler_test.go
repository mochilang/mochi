//go:build slow

package zigt_test

import (
        "bytes"
        "flag"
        "os"
        "os/exec"
        "path/filepath"
        "strings"
        "testing"

	"mochi/parser"
	zigt "mochi/transpiler/x/zig"
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

var update = flag.Bool("update", false, "update golden files")

func TestTranspiler_Golden(t *testing.T) {
        if _, err := exec.LookPath("zig"); err != nil {
                t.Skip("zig not installed")
        }
        root := repoRoot(t)
        goldenDir := filepath.Join(root, "tests", "transpiler", "x", "zig")
        files, err := filepath.Glob(filepath.Join(goldenDir, "*.zig"))
        if err != nil {
                t.Fatal(err)
        }
        if len(files) == 0 {
                t.Fatalf("no golden files in %s", goldenDir)
        }

        for _, zigPath := range files {
                name := strings.TrimSuffix(filepath.Base(zigPath), ".zig")
                src := filepath.Join(root, "tests", "vm", "valid", name+".mochi")
                wantPath := filepath.Join(goldenDir, name+".out")

                t.Run(name, func(t *testing.T) {
                        prog, err := parser.Parse(src)
                        if err != nil {
                                t.Fatalf("parse: %v", err)
                        }
                        env := types.NewEnv(nil)
                        if errs := types.Check(prog, env); len(errs) > 0 {
                                t.Fatalf("type: %v", errs[0])
                        }
                        ast, err := zigt.Transpile(prog, env)
                        if err != nil {
                                t.Fatalf("transpile: %v", err)
                        }
                        code := ast.Emit()
                        if err := os.WriteFile(zigPath, code, 0o644); err != nil {
                                t.Fatalf("write code: %v", err)
                        }
                        cmd := exec.Command("zig", "run", zigPath)
                        out, err := cmd.CombinedOutput()
                        if err != nil {
                                if *update {
                                        _ = os.WriteFile(strings.TrimSuffix(zigPath, ".zig")+".error", out, 0o644)
                                }
                                t.Fatalf("run: %v\n%s", err, out)
                        }
                        if *update {
                                _ = os.Remove(strings.TrimSuffix(zigPath, ".zig")+".error")
                                trimmed := bytes.TrimSpace(out)
                                if err := os.WriteFile(wantPath, trimmed, 0o644); err != nil {
                                        t.Fatalf("write want: %v", err)
                                }
                        }
                        trimmed := bytes.TrimSpace(out)
                        want, err := os.ReadFile(wantPath)
                        if err != nil {
                                t.Fatalf("read want: %v", err)
                        }
                        want = bytes.TrimSpace(want)
                        if !bytes.Equal(trimmed, want) {
                                t.Errorf("output mismatch for %s:\nGot: %s\nWant: %s", name, trimmed, want)
                        }
                })
        }
}
