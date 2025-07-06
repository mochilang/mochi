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
	vm "mochi/runtime/vm"
	"mochi/types"
)

func snippetFromFile(path string) string {
	data, _ := os.ReadFile(path)
	lines := strings.Split(string(data), "\n")
	if len(lines) > 10 {
		lines = lines[:10]
	}
	for i, l := range lines {
		lines[i] = fmt.Sprintf("%3d| %s", i+1, l)
	}
	return strings.Join(lines, "\n")
}

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
			mochiSrc, err := convert(src)
			root := rootDir(t)
			outDir := filepath.Join(root, "tests/any2mochi", lang)
			outPath := filepath.Join(outDir, name+outExt)
			errPath := filepath.Join(outDir, name+errExt)
			renameLegacy(outDir, lang, name, outExt)
			renameLegacy(outDir, lang, name, errExt)

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
						} else {
							if p2, vErr := vm.CompileWithSource(prog, env, string(mochiSrc)); vErr != nil {
								err = fmt.Errorf("vm compile error: %w", vErr)
							} else {
								var buf bytes.Buffer
								m := vm.New(p2, &buf)
								if rErr := m.Run(); rErr != nil {
									if ve, ok := rErr.(*vm.VMError); ok {
										err = fmt.Errorf("vm run error:\n%s", ve.Format(p2))
									} else {
										err = fmt.Errorf("vm run error: %v", rErr)
									}
								}
							}
						}
					}
				}
			}

			if err != nil {
				if *update {
					os.WriteFile(outPath, nil, 0644)
					msg := fmt.Sprintf("%v\n\n%s", err, snippetFromFile(src))
					os.WriteFile(errPath, normalizeOutput(rootDir(t), []byte(msg)), 0644)
				}
				altErr := filepath.Join(outDir, name+"."+lang+errExt)
				want, readErr := readGolden(errPath, altErr)
				if readErr != nil {
					t.Fatalf("missing golden error: %v", readErr)
				}
				msg := fmt.Sprintf("%v\n\n%s", err, snippetFromFile(src))
				if got := normalizeOutput(rootDir(t), []byte(msg)); !bytes.Equal(got, normalizeOutput(rootDir(t), want)) {
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
					msg := fmt.Sprintf("%v\n\n%s", err, snippetFromFile(src))
					os.WriteFile(errPath, normalizeOutput(rootDir(t), []byte(msg)), 0644)
				}
				altErr := filepath.Join(outDir, name+"."+lang+errExt)
				want, readErr := readGolden(errPath, altErr)
				if readErr != nil {
					t.Fatalf("missing golden error: %v", readErr)
				}
				msg := fmt.Sprintf("%v\n\n%s", err, snippetFromFile(src))
				if got := normalizeOutput(rootDir(t), []byte(msg)); !bytes.Equal(got, normalizeOutput(rootDir(t), want)) {
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

// runConvertRunGolden behaves like runConvertGolden but additionally parses,
// type-checks and executes the produced Mochi code. Any runtime error is
// returned to the caller for aggregation.
func runConvertRunGolden(t *testing.T, dir, pattern string, convert func(string) ([]byte, error), lang, outExt, errExt string) []string {
	files, err := filepath.Glob(filepath.Join(dir, pattern))
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no files: %s", filepath.Join(dir, pattern))
	}
	var allErrs []string
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

		var errMsg string
		t.Run(name, func(t *testing.T) {
			out, err := convert(src)
			root := rootDir(t)
			outDir := filepath.Join(root, "tests/any2mochi", lang)
			os.MkdirAll(outDir, 0755)
			outPath := filepath.Join(outDir, name+outExt)
			errPath := filepath.Join(outDir, name+errExt)
			renameLegacy(outDir, lang, name, outExt)
			renameLegacy(outDir, lang, name, errExt)

			if err == nil {
				prog, pErr := parser.ParseString(string(out))
				if pErr != nil {
					err = fmt.Errorf("parse error: %w", pErr)
				} else {
					env := types.NewEnv(nil)
					if errs := types.Check(prog, env); len(errs) > 0 {
						err = fmt.Errorf("type error: %v", errs[0])
					} else if p2, vErr := vm.CompileWithSource(prog, env, string(out)); vErr != nil {
						err = fmt.Errorf("vm compile error: %w", vErr)
					} else {
						var buf bytes.Buffer
						m := vm.New(p2, &buf)
						if rErr := m.Run(); rErr != nil {
							if ve, ok := rErr.(*vm.VMError); ok {
								err = fmt.Errorf("vm run error:\n%s", ve.Format(p2))
							} else {
								err = fmt.Errorf("vm run error: %v", rErr)
							}
						}
					}
				}
			}

			if err != nil {
				fmt.Fprintln(os.Stderr, err)
				errMsg = fmt.Sprintf("%s: %v", name, err)
				if *update {
					os.WriteFile(outPath, nil, 0644)
					msg := fmt.Sprintf("%v\n\n%s", err, snippetFromFile(src))
					os.WriteFile(errPath, normalizeOutput(rootDir(t), []byte(msg)), 0644)
				}
				altErr := filepath.Join(outDir, name+"."+lang+errExt)
				want, readErr := readGolden(errPath, altErr)
				if readErr != nil {
					t.Fatalf("missing golden error: %v", readErr)
				}
				msg := fmt.Sprintf("%v\n\n%s", err, snippetFromFile(src))
				if got := normalizeOutput(rootDir(t), []byte(msg)); !bytes.Equal(got, normalizeOutput(rootDir(t), want)) {
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

		if errMsg != "" {
			allErrs = append(allErrs, errMsg)
		}
	}
	return allErrs
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

// RunConvertRunGolden converts files and also executes the resulting Mochi code.
// Any runtime errors are compared against golden files and returned for
// aggregation.
func RunConvertRunGolden(t *testing.T, dir, pattern string, convert func(string) ([]byte, error), lang, outExt, errExt string) []string {
	return runConvertRunGolden(t, dir, pattern, convert, lang, outExt, errExt)
}

// WriteErrorsMarkdown writes all error messages to ERRORS.md in the provided
// directory.
func WriteErrorsMarkdown(dir string, errs []string) {
	_ = os.MkdirAll(dir, 0755)
	path := filepath.Join(dir, "ERRORS.md")
	var buf strings.Builder
	buf.WriteString("# Errors\n\n")
	if len(errs) == 0 {
		buf.WriteString("None\n")
	} else {
		for _, e := range errs {
			buf.WriteString("- " + e + "\n")
		}
	}
	_ = os.WriteFile(path, []byte(buf.String()), 0644)
}

// FindRepoRoot is an exported wrapper for findRepoRoot.
func FindRepoRoot(t *testing.T) string { return findRepoRoot(t) }
