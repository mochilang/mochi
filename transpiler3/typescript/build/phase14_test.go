package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase14LLMNode/Deno/Bun gate LLM cassette lowering.
//
// Phase 14 lowering contract:
//
//   - `llm_generate(provider, model, prompt)` → `mochi_llm_generate(...)`.
//
//   - A DJB2 XOR key function `mochi_djb2_key` (BigInt-based) is emitted
//     to hash prompts against the cassette.
//
//   - When no cassette directory is set (MOCHI_LLM_CASSETTE_DIR unset),
//     `mochi_llm_generate` returns an empty string without error.
//
// All fixtures run with no cassette → output contains empty-string results.
//
// Minimum fixture count: 8.
func TestPhase14LLMNode(t *testing.T) { runPhase14On(t, "node") }
func TestPhase14LLMDeno(t *testing.T) { runPhase14On(t, "deno") }
func TestPhase14LLMBun(t *testing.T)  { runPhase14On(t, "bun") }

func runPhase14On(t *testing.T, runtime string) {
	t.Helper()
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase14-llm")
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
	if mochiCount < 8 {
		t.Fatalf("Phase 14 fixture corpus has %d .mochi files, expected at least 8", mochiCount)
	}
}

// TestPhase14EmitShape verifies that LLM fixtures include mochi_djb2_key
// and mochi_llm_generate helpers.
func TestPhase14EmitShape(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase14-llm")
	cases := []struct {
		fixture string
		wants   []string
	}{
		{
			fixture: "llm_no_cassette.mochi",
			wants:   []string{"mochi_djb2_key", "mochi_llm_generate"},
		},
		{
			fixture: "llm_provider_model.mochi",
			wants:   []string{"mochi_llm_generate", "mochi_djb2_key"},
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
		})
	}
}
