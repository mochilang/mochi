package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase5Sums walks the Phase 5 sum-type fixtures and exercises
// each through the full PHP transpiler. Tests skip when PHP is not
// installed; CI uses shivammathur/setup-php@v2 to drive the
// end-to-end gate.
func TestPhase5Sums(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "php", "fixtures", "phase05-sums")
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

// TestPhase5EmitFragments asserts the lowerer emits the expected PHP
// shape for the sealed-hierarchy sum-type lowering: one abstract
// readonly base class plus one final readonly variant subclass, with
// match arms becoming an `instanceof` discriminator chain. The base
// must be `abstract readonly` because PHP 8.4 forbids a readonly
// subclass from extending a non-readonly parent. The fragments cover
// the four feature shapes Phase 5.0 ships: variant with int field,
// nullary variants, match-as-expression result temp, and string
// arm bodies.
func TestPhase5EmitFragments(t *testing.T) {
	cases := []struct {
		fixture string
		wants   []string
	}{
		{
			fixture: "sum_basic.mochi",
			wants: []string{
				`abstract readonly class Shape`,
				`final readonly class Shape_Circle extends Shape`,
				`final readonly class Shape_Square extends Shape`,
				`public int $r,`,
				`public int $side,`,
				`new Shape_Circle(r: 5);`,
				`($__mochi_match_1 instanceof Shape_Circle)`,
				`($__mochi_match_1 instanceof Shape_Square)`,
				`$r = $__mochi_match_1->r;`,
				`$side = $__mochi_match_1->side;`,
			},
		},
		{
			fixture: "sum_function.mochi",
			wants: []string{
				// Sum-type param resolves to the abstract base class.
				`function mochi__abs_val(Num $x): int`,
				// Each call site passes a freshly-constructed variant.
				`mochi__abs_val(new Num_Pos(n: 5))`,
				`mochi__abs_val(new Num_Neg(n: 3))`,
				`mochi__abs_val(new Num_Zero())`,
				// Negation of the bound field is preserved through
				// the chained-if lowering.
				`(-$n)`,
			},
		},
		{
			fixture: "sum_nullary.mochi",
			wants: []string{
				`abstract readonly class Color`,
				`final readonly class Color_Red extends Color`,
				`final readonly class Color_Green extends Color`,
				`final readonly class Color_Blue extends Color`,
				// Nullary variants emit an empty constructor.
				`public function __construct() {}`,
				// Instantiation uses the no-arg form.
				`new Color_Green();`,
				`($__mochi_match_1 instanceof Color_Red)`,
				`($__mochi_match_1 instanceof Color_Green)`,
				`($__mochi_match_1 instanceof Color_Blue)`,
			},
		},
		{
			fixture: "sum_string_result.mochi",
			wants: []string{
				`new Color_Blue();`,
				// Match-as-expression materialises a result temp
				// initialised to null, written by each arm, and read
				// by the surrounding let-binding.
				`$__match1 = null;`,
				`$__match1 = 0;`,
				`$__match1 = 1;`,
				`$__match1 = 2;`,
				`$val = $__match1;`,
			},
		},
	}

	for _, c := range cases {
		t.Run(strings.TrimSuffix(c.fixture, ".mochi"), func(t *testing.T) {
			mochiPath := filepath.Join(repoRoot(t), "tests", "transpiler3", "php", "fixtures", "phase05-sums", c.fixture)
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
