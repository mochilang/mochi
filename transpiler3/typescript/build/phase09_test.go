package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase9Agents is the Phase 9 primary gate. Every fixture under
// tests/transpiler3/typescript/fixtures/phase09-agents must lower
// via the TypeScript transpiler and produce byte-equal stdout on
// Node 22, Deno 2, and Bun 1.1 against the recorded .out.
//
// Phase 9 lands Mochi's agent sub-language on TypeScript by mapping
// every `agent NAME { var f: T = init; intent N(...) {...} ... }`
// block to a TS class with mutable fields, a private constructor,
// a static of() factory, and one method per intent. Intent bodies
// run synchronously: `c.intent()` is a regular method call.
//
// The MEP-52 §Phase 9 spec budget (~8 KB gzipped for an async
// runtime engine under `@mochi/runtime/agent` with
// AsyncIterableQueue + AbortController + MochiSupervisor) is
// deferred for the same reason Phase 8 deferred the Datalog
// runtime: every fixture in the Phase 9 corpus is a synchronous
// state machine with no spawn-and-message pattern, no abort
// surface, and no supervision tree. Shipping the runtime engine
// would force the async colour onto every intent (Phase 11) for
// zero behaviour benefit on the corpus. The Rust transpiler made
// the same call at transpiler3/rust/lower/lower.go:246
// (`lowerAgentDecl`); the TS path mirrors it. If a future phase
// introduces `spawn`, mailbox messaging, or supervision, the
// runtime engine can be added without disturbing the synchronous
// path on closed programs.
//
// Phase 9 ships 44 fixtures (the full Rust Phase 9 corpus); the
// floor is 35 per MEP-52 §Phase 9.
func TestPhase9AgentsNode(t *testing.T) { runPhase9FixturesOn(t, "node") }
func TestPhase9AgentsDeno(t *testing.T) { runPhase9FixturesOn(t, "deno") }
func TestPhase9AgentsBun(t *testing.T)  { runPhase9FixturesOn(t, "bun") }

func runPhase9FixturesOn(t *testing.T, runtime string) {
	t.Helper()
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase09-agents")
	entries, err := os.ReadDir(fixtureDir)
	if err != nil {
		t.Fatalf("ReadDir %s: %v", fixtureDir, err)
	}
	mochiCount := 0
	for _, e := range entries {
		if !strings.HasSuffix(e.Name(), ".mochi") {
			continue
		}
		mochiCount++
		name := strings.TrimSuffix(e.Name(), ".mochi")
		t.Run(name, func(t *testing.T) {
			runTsFixture(t, runtime,
				filepath.Join(fixtureDir, e.Name()),
				filepath.Join(fixtureDir, name+".out"))
		})
	}
	if mochiCount < 35 {
		t.Fatalf("Phase 9 fixture corpus has %d .mochi files, expected at least 35", mochiCount)
	}
}

// TestPhase9EmitShape asserts the load-bearing tokens of the agent
// lowering are present in the emit. The invariants encode the
// chosen strategy (synchronous class with mutable fields + method
// dispatch) and rule out the two alternatives that would produce
// the same stdout but break later phases:
//
//   - Async runtime engine. Would surface as imports of
//     `@mochi/runtime/agent`, `AsyncIterableQueue`,
//     `AbortController`, or `MochiAgent`. Forces the async colour
//     onto every intent (Phase 11), inflates every package by 8+
//     KB (Phase 15 budget), and breaks Phase 16 byte-equal repro
//     (runtime version skew shifts emit even when behaviour
//     matches).
//
//   - Free-function lowering with explicit self struct. Would
//     surface as `mochi_agent_NAME__INTENT(__self,` calls. That
//     pattern works for C / Rust (which take `&mut self` by
//     reference) but in TS forces every intent call site to pass
//     the receiver explicitly, doubling the call-site noise for
//     zero gain over `c.intent()`.
//
// What is asserted: the emit declares `class NAME`, the intent
// body reads/writes via `this.X`, and the call site uses the
// `receiver.intent(args)` method-call shape.
func TestPhase9EmitShape(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase09-agents")
	cases := []struct {
		fixture string
		wants   []string
	}{
		{
			fixture: "agent_basic.mochi",
			wants: []string{
				"class Counter {",
				"count: number;",
				"private constructor(opts: { count: number; })",
				"static of(opts: { count: number; }): Counter",
				"increment(): void {",
				"this.count = (this.count + 1);",
				"value(): number {",
				"return this.count;",
				"const c: Counter = Counter.of({ count: 0 });",
				"c.increment();",
				"c.value()",
			},
		},
		{
			fixture: "agent_intent_two_params.mochi",
			wants: []string{
				"class Calc {",
				"muladd(a: number, b: number): void {",
				"this.total = (this.total + (a * b));",
			},
		},
		{
			fixture: "agent_bool.mochi",
			wants: []string{
				"class Switch {",
				"active: boolean;",
				"toggle(): void {",
				"this.active = !(this.active);",
			},
		},
		{
			fixture: "agent_two_agents.mochi",
			wants: []string{
				"class A {",
				"class B {",
				"const a: A = A.of(",
				"const b: B = B.of(",
			},
		},
	}
	for _, tc := range cases {
		t.Run(strings.TrimSuffix(tc.fixture, ".mochi"), func(t *testing.T) {
			outDir := t.TempDir()
			d := &Driver{CacheDir: t.TempDir(), NoCache: true}
			p, err := d.Build(filepath.Join(fixtureDir, tc.fixture), outDir, TargetTypeScriptSource)
			if err != nil {
				t.Fatalf("Build %s: %v", tc.fixture, err)
			}
			src := readTrim(t, p)
			for _, want := range tc.wants {
				if !strings.Contains(src, want) {
					t.Errorf("%s emit missing %q\n---\n%s", tc.fixture, want, src)
				}
			}
		})
	}
}

// TestPhase9NoAsyncRuntime asserts that no async-agent runtime
// engine tokens leak into the TS emit. The synchronous-class path
// must fully consume the agent surface; nothing of the Phase 9
// spec's planned AsyncIterableQueue + AbortController +
// MochiAgent + MochiSupervisor runtime should appear.
//
// If this test fails the most likely cause is that a spec-driven
// future change re-introduced the runtime engine; the audit
// (which dropped the engine in favour of synchronous class
// dispatch) should be re-read first.
func TestPhase9NoAsyncRuntime(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase09-agents")
	entries, err := os.ReadDir(fixtureDir)
	if err != nil {
		t.Fatalf("ReadDir: %v", err)
	}
	forbidden := []string{
		"AsyncIterableQueue",
		"AbortController",
		"MochiAgent",
		"MochiSupervisor",
		"@mochi/runtime/agent",
		"mochi_agent_",
		"AggregateError",
		" await ",
		"async ",
	}
	for _, e := range entries {
		if !strings.HasSuffix(e.Name(), ".mochi") {
			continue
		}
		t.Run(strings.TrimSuffix(e.Name(), ".mochi"), func(t *testing.T) {
			outDir := t.TempDir()
			d := &Driver{CacheDir: t.TempDir(), NoCache: true}
			p, err := d.Build(filepath.Join(fixtureDir, e.Name()), outDir, TargetTypeScriptSource)
			if err != nil {
				t.Fatalf("Build: %v", err)
			}
			src := readTrim(t, p)
			for _, f := range forbidden {
				if strings.Contains(src, f) {
					t.Errorf("%s emit leaked async-runtime token %q\n---\n%s", e.Name(), f, src)
				}
			}
		})
	}
}
