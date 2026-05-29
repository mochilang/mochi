package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase9Agents walks the Phase 9 agent fixtures and exercises each
// through the full PHP transpiler. Tests skip when PHP is not
// installed; CI uses shivammathur/setup-php@v2 to drive the end-to-end
// gate.
func TestPhase9Agents(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "php", "fixtures", "phase09-agents")
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

// TestPhase9EmitFragments asserts that the PHP lowerer emits the
// expected mutable-class shape for each agent feature: simple counter,
// boolean state, string state, float state, multiple intents on one
// agent, intents with parameters, value-returning intents, conditional
// bodies, multi-field agents, and two distinct agents in one program.
//
// Agents lower to `final class NAME` (no readonly) with promoted public
// constructor parameters; intents become `public function name(...)`
// methods whose bodies read and write `$this->FIELD` through the
// `__self->FIELD` sentinel rewrite.
func TestPhase9EmitFragments(t *testing.T) {
	cases := []struct {
		fixture string
		wants   []string
	}{
		{
			fixture: "agent_counter.mochi",
			wants: []string{
				// Agent class header is `final class` (not readonly)
				// so intents can mutate `$this->count`.
				`final class Counter`,
				`public int $count,`,
				// Discard-result intent: $this->count = $this->count + 1.
				`public function increment(): void`,
				`$this->count = ($this->count + 1);`,
				// Value-returning intent reads $this->count.
				`public function value(): int`,
				`return $this->count;`,
				// Construction uses PHP 8 named args.
				`$c = new Counter(count: 0);`,
				// Method calls dispatch through the instance receiver.
				`$c->increment();`,
				`$c->value()`,
			},
		},
		{
			fixture: "agent_bool.mochi",
			wants: []string{
				// `agent Switch` collides with PHP's `switch` reserved
				// word, so the class name is suffixed by phpClassName.
				`final class Switch_`,
				`public bool $active,`,
				// `!active` lowers to `(!$this->active)` (no other rewrite).
				`$this->active = (!$this->active);`,
				`$s = new Switch_(active: false);`,
				`$s->toggle();`,
				`$s->is_on()`,
			},
		},
		{
			fixture: "agent_string.mochi",
			wants: []string{
				`final class Greeter`,
				`public string $name,`,
				// Intent param is plain $n; assignment rewrites the
				// receiver-side to `$this->name`.
				`public function set_name(string $n): void`,
				`$this->name = $n;`,
				`$g = new Greeter(name: "world");`,
				`$g->set_name("mochi");`,
			},
		},
		{
			fixture: "agent_float.mochi",
			wants: []string{
				`final class Balance`,
				`public float $amount,`,
				`$this->amount = ($this->amount + $v);`,
				`$this->amount = ($this->amount - $v);`,
				`$b = new Balance(amount: 0);`,
			},
		},
		{
			fixture: "agent_accumulator.mochi",
			wants: []string{
				// Three intents, one with a parameter, one without args
				// that resets state, one value-returning getter.
				`public function add(int $n): void`,
				`$this->total = ($this->total + $n);`,
				`public function get(): int`,
				`return $this->total;`,
				`$acc = new Accumulator(total: 0);`,
				`$acc->add(10);`,
			},
		},
		{
			fixture: "agent_params.mochi",
			wants: []string{
				// Two-arg intent params propagate as separate PHP params;
				// neither needs `__self->` rewriting since they aren't
				// receiver-bound.
				`public function add(int $a, int $b): void`,
				`$this->result = ($a + $b);`,
				`public function mul(int $a, int $b): void`,
				`$this->result = ($a * $b);`,
				`$calc->add(3, 4);`,
				`$calc->mul(3, 4);`,
			},
		},
		{
			fixture: "agent_nested_call.mochi",
			wants: []string{
				// Intent return value flows into a let binding and then
				// back into a second intent call on the same receiver.
				`public function add(int $x): int`,
				`return ($this->base + $x);`,
				`$r1 = $a->add(5);`,
				`$r2 = $a->add($r1);`,
			},
		},
		{
			fixture: "agent_cond.mochi",
			wants: []string{
				// `if` inside intent body emits a ChainedIfStmt with
				// branches that read $this->limit and return literals.
				`public function check(int $v): bool`,
				`if (($v > $this->limit))`,
				`return true;`,
				`return false;`,
			},
		},
		{
			fixture: "agent_chain.mochi",
			wants: []string{
				// Multi-field agent: two promoted constructor params.
				`public int $top,`,
				`public int $size,`,
				// Semicolon-joined statements in one intent body emit
				// as separate PHP statements.
				`$this->top = $v;`,
				`$this->size = ($this->size + 1);`,
				`$this->size = ($this->size - 1);`,
				`return $this->top;`,
				`$s = new Stack(top: 0, size: 0);`,
			},
		},
		{
			fixture: "agent_two.mochi",
			wants: []string{
				// Two independent agent declarations in one program
				// emit two distinct `final class` blocks. Each carries
				// its own intent set; intents with the same name on
				// different classes don't collide.
				`final class CounterA`,
				`final class CounterB`,
				`$a = new CounterA(n: 0);`,
				`$b = new CounterB(m: 100);`,
				`$a->inc();`,
				`$b->dec();`,
			},
		},
		{
			fixture: "agent_spawn.mochi",
			wants: []string{
				// `spawn AgentType()` synthesizes zero-value field
				// arguments from the AgentDecl; the resulting `new
				// Counter(count: 0)` is shape-equal to the AgentLit
				// form. Subsequent intent calls dispatch through the
				// same instance-method path.
				`final class Counter`,
				`public int $count,`,
				`$c = new Counter(count: 0);`,
				`$c->increment();`,
				`$v = $c->value();`,
			},
		},
	}

	for _, c := range cases {
		t.Run(strings.TrimSuffix(c.fixture, ".mochi"), func(t *testing.T) {
			mochiPath := filepath.Join(repoRoot(t), "tests", "transpiler3", "php", "fixtures", "phase09-agents", c.fixture)
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
