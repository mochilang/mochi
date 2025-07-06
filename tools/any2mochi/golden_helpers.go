//go:build slow

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
	"mochi/runtime/vm"
	"mochi/types"
)

var update = flag.Bool("update", false, "update golden files")

func readGolden(path string, alts ...string) ([]byte, error) {
	if data, err := os.ReadFile(path); err == nil {
		return data, nil
	}
	for _, p := range alts {
		if b, err := os.ReadFile(p); err == nil {
			return b, nil
		}
	}
	return nil, fmt.Errorf("missing golden file: %s", path)
}

func removeIfEmpty(path string) {
	if info, err := os.Stat(path); err == nil && info.Size() == 0 {
		os.Remove(path)
	}
}

func renameLegacy(dir, lang, name, ext string) {
	old := filepath.Join(dir, name+"."+lang+ext)
	new := filepath.Join(dir, name+ext)
	if _, err := os.Stat(old); err == nil {
		if _, err2 := os.Stat(new); err2 == nil {
			os.Remove(old)
		} else {
			os.Rename(old, new)
		}
	}
}

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
		case strings.HasSuffix(src, ".clj.out"):
			name = strings.TrimSuffix(filepath.Base(src), ".clj.out")
		case strings.HasSuffix(src, ".f90.out"):
			name = strings.TrimSuffix(filepath.Base(src), ".f90.out")
		case strings.HasSuffix(src, ".pas.out"):
			name = strings.TrimSuffix(filepath.Base(src), ".pas.out")
		case strings.HasSuffix(src, ".java.out"):
			name = strings.TrimSuffix(filepath.Base(src), ".java.out")
		default:
			name = strings.TrimSuffix(filepath.Base(src), filepath.Ext(src))
		}
		t.Run(name, func(t *testing.T) {
			inputData, _ := os.ReadFile(src)
			mochiSrc, err := convert(src)
			if err != nil {
				err = ErrorWithSnippet(err, string(inputData))
			}
			root := rootDir(t)
			outDir := filepath.Join(root, "tests/any2mochi", lang)
			outPath := filepath.Join(outDir, name+outExt)
			errPath := filepath.Join(outDir, name+errExt)
			renameLegacy(outDir, lang, name, outExt)
			renameLegacy(outDir, lang, name, errExt)

			if err == nil {
				prog, pErr := parser.ParseString(string(mochiSrc))
				if pErr != nil {
					err = ErrorWithSnippet(fmt.Errorf("parse error: %w", pErr), string(mochiSrc))
				} else {
					env := types.NewEnv(nil)
					if errs := types.Check(prog, env); len(errs) > 0 {
						err = ErrorWithSnippet(fmt.Errorf("type error: %v", errs[0]), string(mochiSrc))
					} else {
						if _, cErr := gocode.New(env).Compile(prog); cErr != nil {
							err = ErrorWithSnippet(fmt.Errorf("compile error: %w", cErr), string(mochiSrc))
						} else if p, vmErr := vm.Compile(prog, env); vmErr != nil {
							err = ErrorWithSnippet(fmt.Errorf("vm compile error: %w", vmErr), string(mochiSrc))
						} else {
							var buf bytes.Buffer
							m := vm.New(p, &buf)
							if rErr := m.Run(); rErr != nil {
								err = ErrorWithSnippet(fmt.Errorf("vm run error: %w", rErr), string(mochiSrc))
							}
						}
					}
				}
			}

			if err != nil {
				if *update {
					os.WriteFile(outPath, nil, 0644)
					os.WriteFile(errPath, normalizeOutput(rootDir(t), []byte(err.Error())), 0644)
				}
				altErr := filepath.Join(outDir, name+"."+lang+errExt)
				want, readErr := readGolden(errPath, altErr)
				if readErr != nil {
					t.Fatalf("missing golden error: %v", readErr)
				}
				if got := normalizeOutput(rootDir(t), []byte(err.Error())); !bytes.Equal(got, normalizeOutput(rootDir(t), want)) {
					t.Errorf("error mismatch\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", got, want)
				}
				return
			}

			if *update {
				os.WriteFile(errPath, nil, 0644)
				removeIfEmpty(errPath)
				os.WriteFile(outPath, normalizeOutput(rootDir(t), mochiSrc), 0644)
			}
			altOut := filepath.Join(outDir, name+"."+lang+outExt)
			want, readErr := readGolden(outPath, altOut)
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
		case strings.HasSuffix(src, ".f90.out"):
			name = strings.TrimSuffix(filepath.Base(src), ".f90.out")
		case strings.HasSuffix(src, ".pas.out"):
			name = strings.TrimSuffix(filepath.Base(src), ".pas.out")
		case strings.HasSuffix(src, ".java.out"):
			name = strings.TrimSuffix(filepath.Base(src), ".java.out")
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
			renameLegacy(outDir, lang, name, outExt)
			renameLegacy(outDir, lang, name, errExt)

			if err != nil {
				if *update {
					os.WriteFile(outPath, nil, 0644)
					os.WriteFile(errPath, normalizeOutput(rootDir(t), []byte(err.Error())), 0644)
				}
				altErr := filepath.Join(outDir, name+"."+lang+errExt)
				want, readErr := readGolden(errPath, altErr)
				if readErr != nil {
					t.Fatalf("missing golden error: %v", readErr)
				}
				if got := normalizeOutput(rootDir(t), []byte(err.Error())); !bytes.Equal(got, normalizeOutput(rootDir(t), want)) {
					t.Errorf("error mismatch\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", got, want)
				}
				return
			}

			if *update {
				os.WriteFile(errPath, nil, 0644)
				removeIfEmpty(errPath)
				os.WriteFile(outPath, normalizeOutput(rootDir(t), out), 0644)
			}
			altOut := filepath.Join(outDir, name+"."+lang+outExt)
			want, readErr := readGolden(outPath, altOut)
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

// RunConvertCompileGolden is an exported wrapper for runConvertCompileGolden.
func RunConvertCompileGolden(t *testing.T, dir, pattern string, convert func(string) ([]byte, error), lang, outExt, errExt string) {
	runConvertCompileGolden(t, dir, pattern, convert, lang, outExt, errExt)
}

// RunConvertGolden is an exported wrapper for runConvertGolden.
func RunConvertGolden(t *testing.T, dir, pattern string, convert func(string) ([]byte, error), lang, outExt, errExt string) {
	runConvertGolden(t, dir, pattern, convert, lang, outExt, errExt)
}

// FindRepoRoot is an exported wrapper for findRepoRoot.
func FindRepoRoot(t *testing.T) string { return findRepoRoot(t) }
