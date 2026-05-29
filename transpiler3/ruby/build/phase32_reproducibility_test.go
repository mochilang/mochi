package build

import (
	"bytes"
	"os"
	"path/filepath"
	"sort"
	"testing"
)

// TestPhase32Reproducibility builds the same Mochi source twice via
// Driver.Build and asserts every emitted .rb byte-equals across the two
// runs. This catches non-determinism in lower (map iteration order,
// timestamp embedding, tempdir leakage into source) that would otherwise
// only surface as a flaky publish artefact.
//
// Subtests exercise a small fixture (hello), a cross-cutting fixture that
// touches records / query / sum / closure together, and the gem-target
// build to confirm `.gemspec` content is also deterministic.
func TestPhase32Reproducibility(t *testing.T) {
	if _, err := resolveToolchain(); err != nil {
		t.Skipf("ruby toolchain not available: %v", err)
	}

	cases := []struct {
		name   string
		target Target
		src    string
	}{
		{
			name:   "hello_source",
			target: TargetRubySource,
			src:    "print(\"hi\")\n",
		},
		{
			name:   "records_and_query",
			target: TargetRubySource,
			src: "type User = { name: string, age: int }\n" +
				"let users: list<User> = [\n" +
				"  User { name: \"a\", age: 30 },\n" +
				"  User { name: \"b\", age: 17 },\n" +
				"  User { name: \"c\", age: 45 },\n" +
				"]\n" +
				"let adults = from u in users where u.age >= 18 select u.name\n" +
				"for n in adults {\n  print(n)\n}\n",
		},
		{
			name:   "hello_gem",
			target: TargetRubyGem,
			src:    "print(\"hi gem\")\n",
		},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			srcDir := t.TempDir()
			srcPath := filepath.Join(srcDir, "src.mochi")
			if err := os.WriteFile(srcPath, []byte(tc.src), 0o644); err != nil {
				t.Fatalf("write src: %v", err)
			}

			first := snapshotEmittedFiles(t, srcPath, tc.target)
			second := snapshotEmittedFiles(t, srcPath, tc.target)

			if len(first) == 0 {
				t.Fatalf("first build produced no files")
			}
			if len(first) != len(second) {
				t.Fatalf("file set differs: first=%v second=%v", keys(first), keys(second))
			}
			for name, a := range first {
				b, ok := second[name]
				if !ok {
					t.Fatalf("second build missing %q", name)
				}
				if !bytes.Equal(a, b) {
					t.Fatalf("non-deterministic emission for %q\nfirst=%q\nsecond=%q", name, a, b)
				}
			}
		})
	}
}

func snapshotEmittedFiles(t *testing.T, src string, target Target) map[string][]byte {
	t.Helper()
	out := t.TempDir()
	d := &Driver{CacheDir: t.TempDir()}
	if err := d.Build(src, out, target); err != nil {
		t.Fatalf("Driver.Build: %v", err)
	}
	files := map[string][]byte{}
	err := filepath.Walk(out, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		if info.IsDir() {
			return nil
		}
		rel, err := filepath.Rel(out, path)
		if err != nil {
			return err
		}
		data, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		files[rel] = data
		return nil
	})
	if err != nil {
		t.Fatalf("walk: %v", err)
	}
	return files
}

func keys(m map[string][]byte) []string {
	out := make([]string, 0, len(m))
	for k := range m {
		out = append(out, k)
	}
	sort.Strings(out)
	return out
}
