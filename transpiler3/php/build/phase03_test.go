package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase3Collections walks the Phase 3 fixtures (lists, maps, sets)
// and exercises each through the full PHP transpiler. Tests skip when
// PHP is not installed; CI uses shivammathur/setup-php@v2 to drive the
// end-to-end gate.
//
// list_filter rejoined the set in Phase 6 (closures landed via PR
// #22503's successor); the runner now exercises it end-to-end.
func TestPhase3Collections(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "php", "fixtures", "phase03-collections")
	entries, err := os.ReadDir(fixtureDir)
	if err != nil {
		t.Fatalf("ReadDir %s: %v", fixtureDir, err)
	}
	for _, e := range entries {
		if !strings.HasSuffix(e.Name(), ".mochi") {
			continue
		}
		name := strings.TrimSuffix(e.Name(), ".mochi")
		t.Run(name, func(t *testing.T) {
			runPhpFixture(t,
				filepath.Join(fixtureDir, e.Name()),
				filepath.Join(fixtureDir, name+".out"))
		})
	}
}

// TestPhase3EmitFragments asserts the lowerer produces the expected
// PHP shape for one representative fixture per collection feature so
// regressions surface even when no PHP is installed.
func TestPhase3EmitFragments(t *testing.T) {
	cases := []struct {
		fixture string
		wants   []string
	}{
		{
			fixture: "list_append.mochi",
			wants: []string{
				`$xs = [1, 2, 3];`,
				`$xs = [...$xs, 4];`,
				`foreach ($xs as $x) {`,
			},
		},
		{
			fixture: "list_foreach.mochi",
			wants: []string{
				`$xs = [10, 20, 30];`,
				`foreach ($xs as $x) {`,
			},
		},
		{
			fixture: "list_index.mochi",
			wants: []string{
				`$xs[0]`,
				`$xs[2]`,
			},
		},
		{
			fixture: "list_len.mochi",
			wants: []string{
				`count($xs)`,
			},
		},
		{
			fixture: "map_has.mochi",
			wants: []string{
				`["x" => 10, "y" => 20]`,
				`array_key_exists("x", $m)`,
			},
		},
		{
			fixture: "map_keys.mochi",
			wants: []string{
				`array_keys($m)`,
				`foreach ($ks as $k) {`,
			},
		},
		{
			fixture: "map_len.mochi",
			wants: []string{
				`count($m)`,
				`$m["c"] = 3;`,
			},
		},
		{
			fixture: "map_put_get.mochi",
			wants: []string{
				`$m["c"] = 3;`,
				`$m["a"]`,
				`$m["c"]`,
			},
		},
		{
			fixture: "set_add_has.mochi",
			wants: []string{
				`function mochi_set_make(array $elems): array`,
				`function mochi_set_add(array $s, $e): array`,
				`mochi_set_make([1, 2, 3])`,
				`mochi_set_add($s, 4)`,
				`array_key_exists(2, $s)`,
				`count($s)`,
			},
		},
		{
			fixture: "set_len.mochi",
			wants: []string{
				`mochi_set_make([1, 2, 1])`,
				`mochi_set_add($s, 3)`,
				`count($s)`,
			},
		},
	}

	for _, c := range cases {
		t.Run(strings.TrimSuffix(c.fixture, ".mochi"), func(t *testing.T) {
			mochiPath := filepath.Join(repoRoot(t), "tests", "transpiler3", "php", "fixtures", "phase03-collections", c.fixture)
			if _, err := os.Stat(mochiPath); err != nil {
				t.Skipf("fixture missing: %v", err)
			}
			outDir := t.TempDir()
			d := &Driver{CacheDir: t.TempDir(), NoCache: true}
			p, err := d.Build(mochiPath, outDir, TargetPhpSource)
			if err != nil {
				t.Fatalf("Build(%s): %v", c.fixture, err)
			}
			data, err := os.ReadFile(p)
			if err != nil {
				t.Fatalf("read %s: %v", p, err)
			}
			src := string(data)
			for _, want := range c.wants {
				if !strings.Contains(src, want) {
					t.Errorf("%s: emitted source missing %q\n---\n%s", c.fixture, want, src)
				}
			}
		})
	}
}
