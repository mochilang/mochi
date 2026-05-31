package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase9AgentsNode/Deno/Bun gate agent class lowering on all runtimes.
//
// Phase 9 lowering contract:
//
//   - `agent NAME(field: T) { intent m(): R { ... } }` lowers to a TS
//     `class MochiNAME { private field: T; constructor(field: T) {
//     this.field = field; } m(): R { ... } }`.
//
//   - `NAME(args)` (agent literal) lowers to `new MochiNAME(args)`.
//
//   - `agent_ref.intent_name(args)` lowers to `agent_ref.intent_name(args)`.
//
//   - `__self->field` VarRef inside an intent body is rewritten to `this.field`.
//
//   - Agent intent methods are synchronous in Phase 9 (async intents
//     are a Phase 11 extension).
//
// Minimum fixture count: 15.
func TestPhase9AgentsNode(t *testing.T) { runPhase9On(t, "node") }
func TestPhase9AgentsDeno(t *testing.T) { runPhase9On(t, "deno") }
func TestPhase9AgentsBun(t *testing.T)  { runPhase9On(t, "bun") }

func runPhase9On(t *testing.T, runtime string) {
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
	if mochiCount < 15 {
		t.Fatalf("Phase 9 fixture corpus has %d .mochi files, expected at least 15", mochiCount)
	}
}

// TestPhase9EmitShape verifies the agent class emit shape.
func TestPhase9EmitShape(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase09-agents")
	cases := []struct {
		fixture  string
		wants    []string
		notWants []string
	}{
		{
			fixture: "agent_counter.mochi",
			wants: []string{
				"class MochiCounter",
				"private count: number",
				"constructor(",
				"this.count = count",
			},
			notWants: []string{"__self->", "__e->"},
		},
		{
			fixture: "agent_two_fields.mochi",
			wants: []string{
				"class MochiPoint",
				"private x: number",
				"private y: number",
			},
			notWants: []string{"__self->"},
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
					t.Errorf("%s missing %q\n---\n%s", tc.fixture, want, src)
				}
			}
			for _, notWant := range tc.notWants {
				if strings.Contains(src, notWant) {
					t.Errorf("%s has forbidden %q", tc.fixture, notWant)
				}
			}
		})
	}
}
