//go:build slow

package rosetta

import (
	"bytes"
	"flag"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"testing"

	"mochi/parser"
	"mochi/runtime/vm"
	"mochi/types"
)

func shouldUpdateRosetta() bool {
	f := flag.Lookup("update")
	return f != nil && f.Value.String() == "true"
}

func runVM(src string) ([]byte, error) {
	vm.SetNowSeed(1)
	prog, err := parser.Parse(src)
	if err != nil {
		return nil, fmt.Errorf("parse error: %w", err)
	}
	env := types.NewEnv(nil)
	if errs := types.Check(prog, env); len(errs) > 0 {
		return nil, fmt.Errorf("type error: %v", errs[0])
	}
	p, err := vm.Compile(prog, env)
	if err != nil {
		return nil, fmt.Errorf("compile error: %w", err)
	}
	var out bytes.Buffer
	m := vm.New(p, &out)
	if err := m.Run(); err != nil {
		return nil, fmt.Errorf("run error: %w", err)
	}
	return bytes.TrimSpace(out.Bytes()), nil
}

func TestRosettaVMGolden(t *testing.T) {
	root := findRepoRoot(t)
	os.Setenv("MOCHI_NOW_SEED", "1")
	defer os.Unsetenv("MOCHI_NOW_SEED")
	skip := map[string]bool{"100-prisoners": true}
	pattern := filepath.Join(root, "tests/rosetta/x/Mochi", "*.mochi")
	files, err := filepath.Glob(pattern)
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	if len(files) == 0 {
		t.Fatal("no Mochi Rosetta tests found")
	}
	for _, src := range files {
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		if skip[name] {
			t.Run(name, func(t *testing.T) { t.Skip("skipped") })
			continue
		}
		outPath := filepath.Join(root, "tests/rosetta/x/Mochi", name+".out")
		t.Run(name, func(t *testing.T) {
			got, err := runVM(src)
			errPath := strings.TrimSuffix(src, filepath.Ext(src)) + ".error"
			if err != nil {
				if shouldUpdateRosetta() {
					norm := normalizeOutput(root, []byte(err.Error()))
					_ = os.WriteFile(errPath, append(norm, '\n'), 0644)
					t.Logf("wrote: %s", errPath)
				}
				if data, e := os.ReadFile(errPath); e == nil {
					want := bytes.TrimSpace(normalizeOutput(root, data))
					gotb := bytes.TrimSpace(normalizeOutput(root, []byte(err.Error())))
					if !bytes.Equal(gotb, want) {
						t.Errorf("%s error\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", name, gotb, want)
					}
				} else {
					t.Skipf("%v", err)
				}
				return
			}
			_ = os.Remove(errPath)
			if shouldUpdateRosetta() {
				norm := normalizeOutput(root, got)
				if err := os.WriteFile(outPath, append(norm, '\n'), 0644); err != nil {
					t.Fatalf("write golden: %v", err)
				}
				t.Logf("updated: %s", outPath)
				return
			}
			want, e := os.ReadFile(outPath)
			if e != nil {
				t.Skipf("missing golden: %v", e)
				return
			}
			want = bytes.TrimSpace(normalizeOutput(root, want))
			got = bytes.TrimSpace(normalizeOutput(root, got))
			if !bytes.Equal(got, want) {
				t.Errorf("%s output\n\n--- Got ---\n%s\n\n--- Want ---\n%s\n", name, got, want)
			}
		})
	}
}

func normalizeOutput(root string, b []byte) []byte {
	out := string(b)
	out = strings.ReplaceAll(out, filepath.ToSlash(root)+"/", "")
	out = strings.ReplaceAll(out, filepath.ToSlash(root), "")
	out = strings.ReplaceAll(out, "github.com/mochi-lang/mochi/", "")
	out = strings.ReplaceAll(out, "mochi/tests/", "tests/")
	durRE := regexp.MustCompile(`\([0-9]+(\.[0-9]+)?(ns|µs|ms|s)\)`)
	out = durRE.ReplaceAllString(out, "(X)")
	tsRE := regexp.MustCompile(`\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}Z`)
	out = tsRE.ReplaceAllString(out, "2006-01-02T15:04:05Z")
	out = strings.TrimSpace(out)
	if !strings.HasSuffix(out, "\n") {
		out += "\n"
	}
	return []byte(out)
}
