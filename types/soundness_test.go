package types_test

import (
	"flag"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"sort"
	"strings"
	"testing"

	"mochi/parser"
	"mochi/types"
)

var updateSoundness = flag.Bool("update-soundness", false, "update .expect/.error fixtures from current checker output")

// TestSoundness exercises the MEP-4 §6 fixes. Each fixture has a header
// comment naming the problem number, plus a sibling `.expect` file (asserts
// type check passes) or `.error` file (asserts type check fails with the
// recorded diagnostic). Regressing any fix here means the fixture flips.
func TestSoundness(t *testing.T) {
	dir := filepath.Join("..", "tests", "types", "soundness")
	files, err := filepath.Glob(filepath.Join(dir, "*.mochi"))
	if err != nil {
		t.Fatalf("glob %s: %v", dir, err)
	}
	if len(files) == 0 {
		t.Fatalf("no soundness fixtures found in %s", dir)
	}
	sort.Strings(files)

	for _, src := range files {
		src := src
		name := strings.TrimSuffix(filepath.Base(src), ".mochi")
		t.Run(name, func(t *testing.T) {
			expectPath := strings.TrimSuffix(src, ".mochi") + ".expect"
			errorPath := strings.TrimSuffix(src, ".mochi") + ".error"
			hasExpect := fileExists(expectPath)
			hasError := fileExists(errorPath)
			if hasExpect == hasError {
				t.Fatalf("fixture %s must have exactly one of .expect or .error (have expect=%v error=%v)", filepath.Base(src), hasExpect, hasError)
			}

			prog, err := parser.Parse(src)
			if err != nil {
				t.Fatalf("parse %s: %v", src, err)
			}

			env := types.NewEnv(nil)
			errs := types.Check(prog, env)

			if hasExpect {
				if len(errs) > 0 {
					t.Fatalf("expected type check to pass, got %d error(s):\n%s", len(errs), formatErrs(errs))
				}
				compareOrUpdate(t, expectPath, "✅ Type Check Passed\n")
				return
			}

			if len(errs) == 0 {
				t.Fatalf("expected at least one type error, got none")
			}
			compareOrUpdate(t, errorPath, formatErrs(errs))
		})
	}
}

func fileExists(p string) bool {
	_, err := os.Stat(p)
	return err == nil
}

func formatErrs(errs []error) string {
	var b strings.Builder
	for i, e := range errs {
		fmt.Fprintf(&b, "%2d. %v\n", i+1, e)
	}
	return b.String()
}

// soundnessPathRE strips any absolute path prefix up to the soundness dir so
// fixtures stay portable across checkouts.
var soundnessPathRE = regexp.MustCompile(`[^ \n\t]*tests/types/soundness/`)

func normalizeSoundness(s string) string {
	s = soundnessPathRE.ReplaceAllString(s, "tests/types/soundness/")
	s = strings.TrimRight(s, "\n") + "\n"
	return s
}

func compareOrUpdate(t *testing.T, path, got string) {
	t.Helper()
	got = normalizeSoundness(got)
	if *updateSoundness {
		if err := os.WriteFile(path, []byte(got), 0644); err != nil {
			t.Fatalf("update %s: %v", path, err)
		}
		t.Logf("updated %s", path)
		return
	}
	wantBytes, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read fixture %s: %v (run with -update-soundness to create)", path, err)
	}
	want := normalizeSoundness(string(wantBytes))
	if got != want {
		t.Errorf("mismatch in %s\n--- got ---\n%s--- want ---\n%s", filepath.Base(path), got, want)
	}
}
