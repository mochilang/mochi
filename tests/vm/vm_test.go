//go:build slow

package vm_test

import (
	"bytes"
	"flag"
	"fmt"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"mochi/golden"
	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

func shouldUpdate() bool {
	f := flag.Lookup("update")
	return f != nil && f.Value.String() == "true"
}

func TestVM_Fetch(t *testing.T) {
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Type", "application/json")
		fmt.Fprint(w, `{"msg":"ok"}`)
	}))
	defer srv.Close()

	src := fmt.Sprintf("let r = fetch \"%s\"\nprint(r.msg)", srv.URL)
	prog, err := parser.ParseString(src)
	if err != nil {
		t.Fatal(err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		t.Fatalf("type error: %v", errs[0])
	}
	p, err := vm.Compile(prog, env)
	if err != nil {
		t.Fatal(err)
	}
	var out bytes.Buffer
	m := vm.New(p, &out)
	if err := m.Run(); err != nil {
		t.Fatal(err)
	}
	if strings.TrimSpace(out.String()) != "ok" {
		t.Fatalf("unexpected output: %s", out.String())
	}
}

func TestVM_TPCH(t *testing.T) {
	root := findRepoRoot(t)
	pattern := filepath.Join(root, "tests/dataset/tpc-h", "*.mochi")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no tpc-h source files: %s", pattern)
	}
	found := false
	for _, src := range files {
		base := strings.TrimSuffix(filepath.Base(src), ".mochi")
		want := filepath.Join(root, "tests/dataset/tpc-h/out", base+".out")
		irWant := filepath.Join(root, "tests/dataset/tpc-h/out", base+".ir.out")
		if _, err := os.Stat(want); err != nil {
			continue
		}
		found = true
		name := filepath.Base(src)
		t.Run(name, func(t *testing.T) {
			prog, err := parser.Parse(src)
			if err != nil {
				t.Fatalf("parse error: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				t.Fatalf("type error: %v", errs[0])
			}
			p, err := vm.Compile(prog, env)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			var out bytes.Buffer
			m := vm.New(p, &out)
			if err := m.Run(); err != nil {
				t.Fatalf("run error: %v", err)
			}
			got := strings.TrimSpace(out.String())
			data, err := os.ReadFile(want)
			if err != nil {
				if shouldUpdate() {
					if writeErr := os.WriteFile(want, []byte(got+"\n"), 0644); writeErr == nil {
						t.Logf("updated: %s", want)
					} else {
						t.Fatalf("write golden: %v", writeErr)
					}
				} else {
					t.Fatalf("read golden: %v", err)
				}
			} else {
				wantStr := strings.TrimSpace(string(data))
				if got != wantStr {
					if shouldUpdate() {
						if writeErr := os.WriteFile(want, []byte(got+"\n"), 0644); writeErr == nil {
							t.Logf("updated: %s", want)
						} else {
							t.Fatalf("write golden: %v", writeErr)
						}
					} else {
						t.Errorf("%s\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", name, got, wantStr)
					}
				}
			}

			srcData, err := os.ReadFile(src)
			if err != nil {
				t.Fatalf("read src: %v", err)
			}
			irGot := strings.TrimSpace(p.Disassemble(string(srcData)))
			irData, err := os.ReadFile(irWant)
			if err != nil {
				if shouldUpdate() {
					if writeErr := os.WriteFile(irWant, []byte(irGot+"\n"), 0644); writeErr == nil {
						t.Logf("updated: %s", irWant)
					} else {
						t.Fatalf("write ir golden: %v", writeErr)
					}
				} else {
					t.Fatalf("read ir golden: %v", err)
				}
			} else {
				irWantStr := strings.TrimSpace(string(irData))
				if irGot != irWantStr {
					if shouldUpdate() {
						if writeErr := os.WriteFile(irWant, []byte(irGot+"\n"), 0644); writeErr == nil {
							t.Logf("updated: %s", irWant)
						} else {
							t.Fatalf("write ir golden: %v", writeErr)
						}
					} else {
						t.Errorf("%s IR\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", name, irGot, irWantStr)
					}
				}
			}
		})
	}
	if !found {
		t.Fatal("no tpc-h test files")
	}
}

func TestVM_TPCDS(t *testing.T) {
	root := findRepoRoot(t)
	pattern := filepath.Join(root, "tests/dataset/tpc-ds", "q*.mochi")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no tpc-ds source files: %s", pattern)
	}
	found := false
	for _, src := range files {
		base := strings.TrimSuffix(filepath.Base(src), ".mochi")
		want := filepath.Join(root, "tests/dataset/tpc-ds/out", base+".out")
		irWant := filepath.Join(root, "tests/dataset/tpc-ds/out", base+".ir.out")
		if _, err := os.Stat(want); err != nil {
			continue
		}
		found = true
		name := filepath.Base(src)
		t.Run(name, func(t *testing.T) {
			prog, err := parser.Parse(src)
			if err != nil {
				t.Fatalf("parse error: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				t.Fatalf("type error: %v", errs[0])
			}
			p, err := vm.Compile(prog, env)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			var out bytes.Buffer
			m := vm.New(p, &out)
			if err := m.Run(); err != nil {
				t.Fatalf("run error: %v", err)
			}
			got := strings.TrimSpace(out.String())
			data, err := os.ReadFile(want)
			if err != nil {
				if shouldUpdate() {
					if writeErr := os.WriteFile(want, []byte(got+"\n"), 0644); writeErr == nil {
						t.Logf("updated: %s", want)
					} else {
						t.Fatalf("write golden: %v", writeErr)
					}
				} else {
					t.Fatalf("read golden: %v", err)
				}
			} else {
				wantStr := strings.TrimSpace(string(data))
				if got != wantStr {
					if shouldUpdate() {
						if writeErr := os.WriteFile(want, []byte(got+"\n"), 0644); writeErr == nil {
							t.Logf("updated: %s", want)
						} else {
							t.Fatalf("write golden: %v", writeErr)
						}
					} else {
						t.Errorf("%s\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", name, got, wantStr)
					}
				}
			}

			srcData, err := os.ReadFile(src)
			if err != nil {
				t.Fatalf("read src: %v", err)
			}
			irGot := strings.TrimSpace(p.Disassemble(string(srcData)))
			irData, err := os.ReadFile(irWant)
			if err != nil {
				if shouldUpdate() {
					if writeErr := os.WriteFile(irWant, []byte(irGot+"\n"), 0644); writeErr == nil {
						t.Logf("updated: %s", irWant)
					} else {
						t.Fatalf("write ir golden: %v", writeErr)
					}
				} else {
					t.Fatalf("read ir golden: %v", err)
				}
			} else {
				irWantStr := strings.TrimSpace(string(irData))
				if irGot != irWantStr {
					if shouldUpdate() {
						if writeErr := os.WriteFile(irWant, []byte(irGot+"\n"), 0644); writeErr == nil {
							t.Logf("updated: %s", irWant)
						} else {
							t.Fatalf("write ir golden: %v", writeErr)
						}
					} else {
						t.Errorf("%s IR\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", name, irGot, irWantStr)
					}
				}
			}
		})
	}
	if !found {
		t.Fatal("no tpc-ds test files")
	}
}

func TestVM_JOB(t *testing.T) {
	root := findRepoRoot(t)
	pattern := filepath.Join(root, "tests/dataset/job", "*.mochi")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatalf("no job source files: %s", pattern)
	}
	found := false
	for _, src := range files {
		base := strings.TrimSuffix(filepath.Base(src), ".mochi")
		want := filepath.Join(root, "tests/dataset/job/out", base+".out")
		irWant := filepath.Join(root, "tests/dataset/job/out", base+".ir.out")
		if _, err := os.Stat(want); err != nil {
			continue
		}
		found = true
		name := filepath.Base(src)
		t.Run(name, func(t *testing.T) {
			prog, err := parser.Parse(src)
			if err != nil {
				t.Fatalf("parse error: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				t.Fatalf("type error: %v", errs[0])
			}
			p, err := vm.Compile(prog, env)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			var out bytes.Buffer
			m := vm.New(p, &out)
			if err := m.Run(); err != nil {
				t.Fatalf("run error: %v", err)
			}
			got := strings.TrimSpace(out.String())
			data, err := os.ReadFile(want)
			if err != nil {
				if shouldUpdate() {
					if writeErr := os.WriteFile(want, []byte(got+"\n"), 0644); writeErr == nil {
						t.Logf("updated: %s", want)
					} else {
						t.Fatalf("write golden: %v", writeErr)
					}
				} else {
					t.Fatalf("read golden: %v", err)
				}
			} else {
				wantStr := strings.TrimSpace(string(data))
				if got != wantStr {
					if shouldUpdate() {
						if writeErr := os.WriteFile(want, []byte(got+"\n"), 0644); writeErr == nil {
							t.Logf("updated: %s", want)
						} else {
							t.Fatalf("write golden: %v", writeErr)
						}
					} else {
						t.Errorf("%s\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", name, got, wantStr)
					}
				}
			}

			srcData, err := os.ReadFile(src)
			if err != nil {
				t.Fatalf("read src: %v", err)
			}
			irGot := strings.TrimSpace(p.Disassemble(string(srcData)))
			irData, err := os.ReadFile(irWant)
			if err != nil {
				if shouldUpdate() {
					if writeErr := os.WriteFile(irWant, []byte(irGot+"\n"), 0644); writeErr == nil {
						t.Logf("updated: %s", irWant)
					} else {
						t.Fatalf("write ir golden: %v", writeErr)
					}
				} else {
					t.Fatalf("read ir golden: %v", err)
				}
			} else {
				irWantStr := strings.TrimSpace(string(irData))
				if irGot != irWantStr {
					if shouldUpdate() {
						if writeErr := os.WriteFile(irWant, []byte(irGot+"\n"), 0644); writeErr == nil {
							t.Logf("updated: %s", irWant)
						} else {
							t.Fatalf("write ir golden: %v", writeErr)
						}
					} else {
						t.Errorf("%s IR\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", name, irGot, irWantStr)
					}
				}
			}
		})
	}
	if !found {
		t.Fatal("no job test files")
	}
}

func TestVM_SLT(t *testing.T) {
	root := findRepoRoot(t)
	patterns := []string{
		filepath.Join(root, "tests/dataset/slt/out/select1", "*.mochi"),
		filepath.Join(root, "tests/dataset/slt/out/evidence/slt_lang_update", "*.mochi"),
	}
	var files []string
	for _, p := range patterns {
		list, err := filepath.Glob(p)
		if err != nil {
			t.Fatal(err)
		}
		files = append(files, list...)
	}
	if len(files) == 0 {
		t.Fatalf("no slt source files")
	}
	for _, src := range files {
		want := strings.TrimSuffix(src, ".mochi") + ".out"
		if _, err := os.Stat(want); err != nil {
			continue
		}
		name := filepath.Base(src)
		t.Run(name, func(t *testing.T) {
			prog, err := parser.Parse(src)
			if err != nil {
				t.Fatalf("parse error: %v", err)
			}
			env := types.NewEnv(nil)
			if errs := types.Check(prog, env); len(errs) > 0 {
				t.Fatalf("type error: %v", errs[0])
			}
			p, err := vm.Compile(prog, env)
			if err != nil {
				t.Fatalf("compile error: %v", err)
			}
			var out bytes.Buffer
			m := vm.New(p, &out)
			if err := m.Run(); err != nil {
				t.Fatalf("run error: %v", err)
			}
			got := strings.TrimSpace(out.String())
			data, err := os.ReadFile(want)
			if err != nil {
				if shouldUpdate() {
					if writeErr := os.WriteFile(want, []byte(got+"\n"), 0644); writeErr == nil {
						t.Logf("updated: %s", want)
					} else {
						t.Fatalf("write golden: %v", writeErr)
					}
				} else {
					t.Fatalf("read golden: %v", err)
				}
			} else {
				wantStr := strings.TrimSpace(string(data))
				if got != wantStr {
					if shouldUpdate() {
						if writeErr := os.WriteFile(want, []byte(got+"\n"), 0644); writeErr == nil {
							t.Logf("updated: %s", want)
						} else {
							t.Fatalf("write golden: %v", writeErr)
						}
					} else {
						t.Errorf("%s\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", name, got, wantStr)
					}
				}
			}
		})
	}
}

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
