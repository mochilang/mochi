package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase4Records walks the Phase 4 record fixtures and exercises
// each through the full PHP transpiler. Tests skip when PHP is not on
// PATH; CI runs the full pipeline via shivammathur/setup-php@v2.
func TestPhase4Records(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "php", "fixtures", "phase04-records")
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

// TestPhase4EmitFragments asserts the lowerer produces the expected PHP
// for one representative shape per record feature so regressions surface
// without needing PHP installed.
func TestPhase4EmitFragments(t *testing.T) {
	cases := []struct {
		fixture string
		wants   []string
	}{
		{
			fixture: "record_basic.mochi",
			wants: []string{
				`final readonly class Pt`,
				`public int $x,`,
				`public int $y,`,
				`$p = new Pt(x: 1, y: 2);`,
				`$p->x`,
				`$p->y`,
			},
		},
		{
			fixture: "record_eq_true.mochi",
			wants: []string{
				`($a == $b)`,
			},
		},
		{
			fixture: "record_eq_false.mochi",
			wants: []string{
				`($a == $b)`,
				`($a != $b)`,
			},
		},
		{
			fixture: "record_fn_arg.mochi",
			wants: []string{
				`function mochi__sum(Pt $p): int`,
				`($p->x + $p->y)`,
			},
		},
		{
			fixture: "record_fn_return.mochi",
			wants: []string{
				`function mochi__make_pt(int $a, int $b): Pt`,
				`return new Pt(x: $a, y: $b);`,
			},
		},
		{
			fixture: "record_in_list.mochi",
			wants: []string{
				`[new Pt(x: 1, y: 2), new Pt(x: 3, y: 4)]`,
				`$pts[0]`,
				`$pts[1]`,
			},
		},
		{
			fixture: "record_two_types.mochi",
			wants: []string{
				`final readonly class Pt`,
				`final readonly class Color`,
				`new Color(r: 255, g: 128, b: 0)`,
			},
		},
		{
			fixture: "record_in_if.mochi",
			wants: []string{
				`if (($p->x > 5)) {`,
				`if (($p->y < 10)) {`,
			},
		},
		{
			fixture: "record_var_reassign.mochi",
			wants: []string{
				`$p = new Pt(x: 1);`,
				`$p = new Pt(x: 99);`,
			},
		},
		{
			fixture: "record_single_field.mochi",
			wants: []string{
				// Lock the class name and the lone field so a
				// regression that dropped either is caught.
				`final readonly class Box`,
				`public int $value,`,
				`new Box(value: 42);`,
			},
		},
		{
			fixture: "record_bool_field.mochi",
			wants: []string{
				`final readonly class Flag`,
				`public bool $active,`,
				`public int $count,`,
				`new Flag(active: true, count: 5);`,
			},
		},
		{
			fixture: "record_float_field.mochi",
			wants: []string{
				`final readonly class Vec`,
				`public float $dx,`,
				`public float $dy,`,
				`new Vec(dx: 1.5, dy: 2.5);`,
			},
		},
		{
			fixture: "record_string_field.mochi",
			wants: []string{
				`final readonly class Msg`,
				`public string $text,`,
				`public int $count,`,
				`new Msg(text: "hello", count: 3);`,
			},
		},
		{
			fixture: "record_field_arith.mochi",
			wants: []string{
				// Lock the actual arithmetic shapes so a regression
				// that swapped the operands or dropped the field
				// reference would surface. A bare `($` substring is
				// too loose; any binary expression in any record
				// program would satisfy it.
				`$area = ($r->w * $r->h);`,
				`mochi_print_i64(($r->w + $r->h));`,
			},
		},
	}

	for _, c := range cases {
		t.Run(strings.TrimSuffix(c.fixture, ".mochi"), func(t *testing.T) {
			mochiPath := filepath.Join(repoRoot(t), "tests", "transpiler3", "php", "fixtures", "phase04-records", c.fixture)
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
