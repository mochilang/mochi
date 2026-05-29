package build

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase13LLM is the Phase 13 primary gate. Every fixture
// under tests/transpiler3/typescript/fixtures/phase13-llm/<name>/
// must lower via the TypeScript transpiler and produce byte-equal
// stdout on Node 22, Deno 2, and Bun 1.1 against the recorded .out
// when the cassette directory is supplied via MOCHI_LLM_CASSETTE_DIR.
//
// Phase 13 lands Mochi's `generate <provider> { ... }` surface by
// emitting an inline `mochi_llm_generate(provider, prompt)` helper
// that SHA-256s the `provider:prompt` key, reads <dir>/<sha>.txt,
// trims trailing whitespace, and returns the response. The helper
// imports `createHash` from `node:crypto` and `readFileSync` from
// `node:fs`; all three tier-1 runtimes implement those specifiers.
//
// The MEP-52 §Phase 13 spec proposed a multi-provider live HTTP
// dispatch (OpenAI, Anthropic, Google, Llama with per-provider
// request shape and retry). The audit found every fixture in the
// 11-fixture corpus is a deterministic cassette replay; the shared
// rust runtime takes the same path. Live-provider dispatch remains
// available as future 13.1 / 13.2 / 13.3 / 13.4 sub-phases when
// fixtures land that exercise real network calls.
//
// Phase 13 ships 11 fixtures (the full Rust Phase 13 corpus); the
// floor is 10 per MEP-52 §Phase 13.
func TestPhase13LLMNode(t *testing.T) { runPhase13FixturesOn(t, "node") }
func TestPhase13LLMDeno(t *testing.T) { runPhase13FixturesOn(t, "deno") }
func TestPhase13LLMBun(t *testing.T)  { runPhase13FixturesOn(t, "bun") }

func runPhase13FixturesOn(t *testing.T, runtime string) {
	t.Helper()
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase13-llm")
	entries, err := os.ReadDir(fixtureDir)
	if err != nil {
		t.Fatalf("ReadDir %s: %v", fixtureDir, err)
	}
	count := 0
	for _, e := range entries {
		if !e.IsDir() {
			continue
		}
		name := e.Name()
		mochiPath := filepath.Join(fixtureDir, name, name+".mochi")
		wantPath := filepath.Join(fixtureDir, name, name+".out")
		if _, err := os.Stat(mochiPath); err != nil {
			continue
		}
		count++
		t.Run(name, func(t *testing.T) {
			cassetteDir := filepath.Join(fixtureDir, name, "cassette")
			env := []string{"MOCHI_LLM_CASSETTE_DIR=" + cassetteDir}
			runTsFixtureWithEnv(t, runtime, mochiPath, wantPath, env)
		})
	}
	if count < 10 {
		t.Fatalf("Phase 13 fixture corpus has %d fixtures, expected at least 10", count)
	}
}

// TestPhase13EmitShape asserts the load-bearing tokens of the
// cassette-replay lowering are present in the emit. The invariants
// encode the chosen strategy (compile-time inline helper plus
// node:crypto + node:fs imports) and rule out two alternatives that
// would produce the same stdout but break later phases:
//
//   - Live HTTP dispatch via fetch + per-provider request body.
//     Would surface as `await fetch("https://api.openai.com/...")`
//     and an async colour pass over every caller. Forfeits byte-
//     equal determinism (real responses vary) and inflates emit by
//     a runtime that no fixture exercises.
//
//   - Compile-time SHA-256 of the prompt + inline string literal
//     of the response. Would surface as `const r: string = "Hello!";`
//     with no helper, no import. The audit rejected this because it
//     ties the emit to the cassette directory at compile time (the
//     env-var indirection is what the test harness uses to swap
//     cassettes per fixture).
func TestPhase13EmitShape(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase13-llm")
	cases := []struct {
		fixture string
		wants   []string
	}{
		{
			fixture: filepath.Join("generate_hello", "generate_hello.mochi"),
			wants: []string{
				`import { createHash } from "node:crypto";`,
				`import { readFileSync } from "node:fs";`,
				"function mochi_llm_generate(provider: string, prompt: string): string",
				`mochi_llm_generate("openai", "Say hello.")`,
			},
		},
		{
			fixture: filepath.Join("generate_anthropic", "generate_anthropic.mochi"),
			wants: []string{
				`mochi_llm_generate("anthropic", "Count to 3.")`,
			},
		},
		{
			fixture: filepath.Join("generate_with_model", "generate_with_model.mochi"),
			wants: []string{
				`mochi_llm_generate("openai", "Say hi.")`,
			},
		},
	}
	for _, tc := range cases {
		t.Run(strings.TrimSuffix(filepath.Base(tc.fixture), ".mochi"), func(t *testing.T) {
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

// TestPhase13NoLiveProvider asserts that no live-provider tokens
// leak into the TS emit. The cassette-replay path must fully
// consume the `generate <p> { ... }` surface; nothing from the
// Phase 13 spec's originally planned multi-provider HTTP dispatch
// should appear.
//
// If this test fails the most likely cause is that a spec-driven
// future change re-introduced live HTTP dispatch; the audit (which
// dropped the provider engine in favour of cassette replay) should
// be re-read first.
func TestPhase13NoLiveProvider(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase13-llm")
	entries, err := os.ReadDir(fixtureDir)
	if err != nil {
		t.Fatalf("ReadDir: %v", err)
	}
	forbidden := []string{
		"api.openai.com",
		"api.anthropic.com",
		"generativelanguage.googleapis.com",
		"127.0.0.1:11434",
		"localhost:11434",
		"await fetch(",
		"new XMLHttpRequest(",
	}
	for _, e := range entries {
		if !e.IsDir() {
			continue
		}
		name := e.Name()
		mochiPath := filepath.Join(fixtureDir, name, name+".mochi")
		if _, err := os.Stat(mochiPath); err != nil {
			continue
		}
		t.Run(name, func(t *testing.T) {
			outDir := t.TempDir()
			d := &Driver{CacheDir: t.TempDir(), NoCache: true}
			p, err := d.Build(mochiPath, outDir, TargetTypeScriptSource)
			if err != nil {
				t.Fatalf("Build: %v", err)
			}
			src := readTrim(t, p)
			for _, f := range forbidden {
				if strings.Contains(src, f) {
					t.Errorf("%s emit leaked live-provider token %q\n---\n%s", mochiPath, f, src)
				}
			}
		})
	}
}

// runTsFixtureWithEnv is runTsFixture extended to accept additional
// env vars (e.g. MOCHI_LLM_CASSETTE_DIR for Phase 13) and to grant
// Deno the --allow-read + --allow-env flags it needs to invoke
// node:fs.readFileSync and read process env. Node and Bun are
// permissive by default; Deno's default-deny model is the only one
// that needs the explicit grants.
func runTsFixtureWithEnv(t *testing.T, runtime, mochiPath, wantFile string, extraEnv []string) {
	t.Helper()
	bin, ok := resolveRuntime(runtime)
	if !ok {
		t.Skipf("%s not on PATH", runtime)
	}
	want, err := os.ReadFile(wantFile)
	if err != nil {
		t.Fatalf("read want %s: %v", wantFile, err)
	}
	outDir := t.TempDir()
	d := &Driver{CacheDir: t.TempDir(), NoCache: true}
	emittedPath, err := d.Build(mochiPath, outDir, TargetTypeScriptSource)
	if err != nil {
		t.Fatalf("Build(%s): %v", filepath.Base(mochiPath), err)
	}
	args := runtimeArgsWithPerms(runtime, emittedPath)
	cmd := exec.Command(bin, args...)
	cmd.Env = append(os.Environ(), extraEnv...)
	var stdout bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		t.Fatalf("%s %s: %v", runtime, emittedPath, err)
	}
	got := stdout.Bytes()
	if !bytes.Equal(got, want) {
		t.Errorf("%s stdout mismatch\ngot:  %q\nwant: %q", runtime, got, want)
	}
}

// runtimeArgsWithPerms is runtimeArgs extended to pass the Deno
// permission flags that fs + env access require. Node and Bun are
// unchanged from runtimeArgs.
func runtimeArgsWithPerms(runtime, path string) []string {
	switch runtime {
	case "node":
		return []string{path}
	case "deno":
		return []string{"run", "--allow-read", "--allow-env", path}
	case "bun":
		return []string{path}
	default:
		return []string{path}
	}
}
