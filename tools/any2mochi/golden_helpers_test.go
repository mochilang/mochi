package any2mochi

import (
	"bytes"
	"flag"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"

	gocode "mochi/compile/go"
	"mochi/parser"
	"mochi/types"
)

var update = flag.Bool("update", false, "update golden files")

func runConvertCompileGolden(t *testing.T, dir, pattern string, convert func(string) ([]byte, error), lang, outExt, errExt string) {
	files, err := filepath.Glob(filepath.Join(dir, pattern))
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", filepath.Join(dir, pattern))
	}
	for _, src := range files {
		var name string
		switch {
		case strings.HasSuffix(src, ".go.out"):
			name = strings.TrimSuffix(filepath.Base(src), ".go.out")
		case strings.HasSuffix(src, ".py.out"):
			name = strings.TrimSuffix(filepath.Base(src), ".py.out")
		case strings.HasSuffix(src, ".ts.out"):
			name = strings.TrimSuffix(filepath.Base(src), ".ts.out")
		default:
			name = strings.TrimSuffix(filepath.Base(src), filepath.Ext(src))
		}
		t.Run(name, func(t *testing.T) {
			mochiSrc, err := convert(src)
			root := rootDir(t)
			outPath := filepath.Join(root, "tests/any2mochi", lang, name+outExt)
			errPath := filepath.Join(root, "tests/any2mochi", lang, name+errExt)

			if err == nil {
				prog, pErr := parser.ParseString(string(mochiSrc))
				if pErr != nil {
					err = fmt.Errorf("parse error: %w", pErr)
				} else {
					env := types.NewEnv(nil)
					if errs := types.Check(prog, env); len(errs) > 0 {
						err = fmt.Errorf("type error: %v", errs[0])
					} else {
						if _, cErr := gocode.New(env).Compile(prog); cErr != nil {
							err = fmt.Errorf("compile error: %w", cErr)
						}
					}
				}
			}

			if err != nil {
				if *update {
					os.WriteFile(outPath, nil, 0644)
					os.WriteFile(errPath, normalizeOutput(rootDir(t), []byte(err.Error())), 0644)
				}
				want, readErr := os.ReadFile(errPath)
				if readErr != nil {
					t.Fatalf("missing golden error: %v", readErr)
				}
				if got := normalizeOutput(rootDir(t), []byte(err.Error())); !bytes.Equal(got, normalizeOutput(rootDir(t), want)) {
					t.Errorf("error mismatch\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", got, want)
				}
				return
			}

			if *update {
				os.Remove(errPath)
				os.WriteFile(outPath, normalizeOutput(rootDir(t), mochiSrc), 0644)
			}
			want, readErr := os.ReadFile(outPath)
			if readErr != nil {
				t.Fatalf("missing golden output: %v", readErr)
			}
			if got := normalizeOutput(rootDir(t), bytes.TrimSpace(mochiSrc)); !bytes.Equal(got, normalizeOutput(rootDir(t), want)) {
				t.Errorf("golden mismatch\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", got, want)
			}
		})
	}
}

func runConvertGolden(t *testing.T, dir, pattern string, convert func(string) ([]byte, error), lang, outExt, errExt string) {
	files, err := filepath.Glob(filepath.Join(dir, pattern))
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", filepath.Join(dir, pattern))
	}
	for _, src := range files {
		var name string
		switch {
		case strings.HasSuffix(src, ".go.out"):
			name = strings.TrimSuffix(filepath.Base(src), ".go.out")
		case strings.HasSuffix(src, ".py.out"):
			name = strings.TrimSuffix(filepath.Base(src), ".py.out")
		case strings.HasSuffix(src, ".ts.out"):
			name = strings.TrimSuffix(filepath.Base(src), ".ts.out")
		default:
			name = strings.TrimSuffix(filepath.Base(src), filepath.Ext(src))
		}
		t.Run(name, func(t *testing.T) {
			out, err := convert(src)
			root := rootDir(t)
			outDir := filepath.Join(root, "tests/any2mochi", lang)
			os.MkdirAll(outDir, 0755)
			outPath := filepath.Join(outDir, name+outExt)
			errPath := filepath.Join(outDir, name+errExt)

			if err != nil {
				if *update {
					os.WriteFile(outPath, nil, 0644)
					os.WriteFile(errPath, normalizeOutput(rootDir(t), []byte(err.Error())), 0644)
				}
				want, readErr := os.ReadFile(errPath)
				if readErr != nil {
					t.Fatalf("missing golden error: %v", readErr)
				}
				if got := normalizeOutput(rootDir(t), []byte(err.Error())); !bytes.Equal(got, normalizeOutput(rootDir(t), want)) {
					t.Errorf("error mismatch\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", got, want)
				}
				return
			}

			if *update {
				os.Remove(errPath)
				os.WriteFile(outPath, normalizeOutput(rootDir(t), out), 0644)
			}
			want, readErr := os.ReadFile(outPath)
			if readErr != nil {
				t.Fatalf("missing golden output: %v", readErr)
			}
			if got := normalizeOutput(rootDir(t), bytes.TrimSpace(out)); !bytes.Equal(got, normalizeOutput(rootDir(t), want)) {
				t.Errorf("golden mismatch\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", got, want)
			}
		})
	}
}

func rootDir(t *testing.T) string { return findRepoRoot(t) }

func findRepoRoot(t *testing.T) string {
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
	t.Fatal("go.mod not found (not in Go module)")
	return ""
}

func normalizeOutput(root string, b []byte) []byte {
	out := string(b)
	out = strings.ReplaceAll(out, filepath.ToSlash(root)+"/", "")
	out = strings.ReplaceAll(out, filepath.ToSlash(root), "")
	out = strings.ReplaceAll(out, "github.com/mochi-lang/mochi/", "")
	out = strings.ReplaceAll(out, "mochi/tests/", "tests/")
	out = strings.TrimSpace(out)
	if !strings.HasSuffix(out, "\n") {
		out += "\n"
	}
	return []byte(out)
}
