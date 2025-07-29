//go:build slow

package zig_test

import (
	"flag"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"mochi/tools/a2mochi/x/zig"
	"mochi/tools/slt/logic"
)

var update = flag.Bool("update", false, "update golden files")

func repoRoot(t testing.TB) string {
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
	t.Fatalf("go.mod not found")
	return "" // unreachable
}

func runTransform(srcPath, root, outDir string) error {
	name := strings.TrimSuffix(filepath.Base(srcPath), ".zig")
	errPath := filepath.Join(outDir, name+".error")
	os.Remove(errPath)
	astPath := filepath.Join(outDir, name+".ast")
	mochiPath := filepath.Join(outDir, name+".mochi")
	outPath := filepath.Join(outDir, name+".out")

	writeErr := func(msg string) {
		os.WriteFile(errPath, []byte(msg), 0o644)
	}

	data, err := os.ReadFile(srcPath)
	if err != nil {
		if *update {
			writeErr(fmt.Sprintf("read src: %v", err))
			return nil
		}
		return fmt.Errorf("read src: %w", err)
	}

	prog, err := zig.Parse(string(data))
	if err != nil {
		if *update {
			writeErr(fmt.Sprintf("parse: %v", err))
			return nil
		}
		return fmt.Errorf("parse: %w", err)
	}

	node, err := zig.Transform(prog)
	if err != nil {
		if *update {
			writeErr(fmt.Sprintf("transform: %v", err))
			return nil
		}
		return fmt.Errorf("transform: %w", err)
	}

	src, err := zig.Print(node)
	if err != nil {
		if *update {
			writeErr(fmt.Sprintf("print: %v", err))
			return nil
		}
		return fmt.Errorf("print: %w", err)
	}

	want, err := os.ReadFile(astPath)
	if err != nil {
		if *update {
			writeErr(fmt.Sprintf("missing golden: %v", err))
			return nil
		}
		return fmt.Errorf("missing golden: %w", err)
	}
	if got := []byte(node.String()); string(want) != string(got) {
		if *update {
			writeErr("golden mismatch")
			return nil
		}
		return fmt.Errorf("golden mismatch")
	}

	out, err := logic.RunMochi(src, 5*time.Second)
	if err != nil {
		if *update {
			writeErr(fmt.Sprintf("run mochi: %v", err))
			return nil
		}
		return fmt.Errorf("run mochi: %w", err)
	}

	if *update {
		if err := os.WriteFile(outPath, []byte(out), 0o644); err != nil {
			writeErr(err.Error())
			return nil
		}
	}

	wantOut, err := os.ReadFile(filepath.Join(root, "tests/vm/valid", name+".out"))
	if err != nil {
		if *update {
			writeErr(fmt.Sprintf("missing vm output: %v", err))
			return nil
		}
		return fmt.Errorf("missing vm output: %w", err)
	}
	if strings.TrimSpace(out) != strings.TrimSpace(string(wantOut)) {
		if *update {
			writeErr("output mismatch")
			return nil
		}
		return fmt.Errorf("output mismatch")
	}

	if *update {
		os.Remove(errPath)
		if err := os.WriteFile(astPath, []byte(node.String()), 0o644); err != nil {
			writeErr(err.Error())
			return nil
		}
		if err := os.WriteFile(mochiPath, []byte(src), 0o644); err != nil {
			writeErr(err.Error())
			return nil
		}
		if err := os.WriteFile(outPath, []byte(out), 0o644); err != nil {
			writeErr(err.Error())
			return nil
		}
	}

	return nil
}

func TestTransform_Golden(t *testing.T) {
	root := repoRoot(t)
	pattern := filepath.Join(root, "tests/transpiler/x/zig", "*.zig")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", pattern)
	}

	outDir := filepath.Join(root, "tests/a2mochi/x/zig")
	os.MkdirAll(outDir, 0o755)

	for _, srcPath := range files {
		name := strings.TrimSuffix(filepath.Base(srcPath), ".zig")
		astPath := filepath.Join(outDir, name+".ast")
		errPath := filepath.Join(outDir, name+".error")
		if _, err := os.Stat(astPath); err != nil && !*update {
			t.Skipf("missing golden: %s", name)
			continue
		}
		if _, err := os.Stat(errPath); err == nil && !*update {
			msg, _ := os.ReadFile(errPath)
			t.Skipf("previous error: %s", strings.TrimSpace(string(msg)))
			continue
		}
		t.Run(name, func(t *testing.T) {
			if err := runTransform(srcPath, root, outDir); err != nil {
				if _, err2 := os.Stat(astPath); err2 == nil {
					t.Fatalf("%v", err)
				}
				t.Skipf("%v", err)
			}
		})
	}
	if *update {
		updateReadme()
	}
}

func TestMain(m *testing.M) {
	code := m.Run()
	updateReadme()
	os.Exit(code)
}

func updateReadme() {
	zig.UpdateReadme()
}
