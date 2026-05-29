package build

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase13LLM walks the Phase 13 LLM fixtures and exercises each
// through the full PHP transpiler, then runs the result with
// MOCHI_LLM_CASSETTE_DIR pointing at the per-fixture cassette folder.
// Tests skip when PHP is not installed; CI uses
// shivammathur/setup-php@v2 to drive the end-to-end gate.
func TestPhase13LLM(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "php", "fixtures", "phase13-llm")
	entries, err := os.ReadDir(fixtureDir)
	if err != nil {
		t.Fatalf("ReadDir %s: %v", fixtureDir, err)
	}
	for _, e := range entries {
		if !e.IsDir() {
			continue
		}
		name := e.Name()
		t.Run(name, func(t *testing.T) {
			mochi := filepath.Join(fixtureDir, name, name+".mochi")
			want := filepath.Join(fixtureDir, name, name+".out")
			cassette := filepath.Join(fixtureDir, name, "cassette")
			runPhpLLMFixture(t, mochi, want, cassette)
		})
	}
}

// runPhpLLMFixture is like runPhpFixture but sets MOCHI_LLM_CASSETTE_DIR
// so the runtime helper finds the pre-recorded response.
func runPhpLLMFixture(t *testing.T, mochiPath, wantFile, cassetteDir string) {
	t.Helper()
	if _, err := exec.LookPath("php"); err != nil {
		if p := os.Getenv("PHP_PATH"); p == "" {
			t.Skipf("php not on PATH: %v", err)
		}
	}

	want, err := os.ReadFile(wantFile)
	if err != nil {
		t.Fatalf("read want file %s: %v", wantFile, err)
	}

	outDir := t.TempDir()
	d := &Driver{CacheDir: t.TempDir(), NoCache: true}
	emittedPath, err := d.Build(mochiPath, outDir, TargetPhpSource)
	if err != nil {
		t.Fatalf("Build(%s): %v", filepath.Base(mochiPath), err)
	}

	cmd := exec.Command("php", emittedPath)
	cmd.Env = append(os.Environ(), "MOCHI_LLM_CASSETTE_DIR="+cassetteDir)
	var stdout bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		t.Fatalf("run %s: %v", emittedPath, err)
	}

	got := stdout.Bytes()
	if !bytes.Equal(got, want) {
		t.Errorf("stdout mismatch\ngot:  %q\nwant: %q", got, want)
	}
}

// TestPhase13EmitFragments asserts that the PHP lowerer emits the
// expected cassette-backed LLM helper shape for each provider/prompt
// combo: bare `mochi_llm_generate(provider, model, prompt)` call sites
// in user code, plus the inline runtime that performs DJB2 lookup
// against MOCHI_LLM_CASSETTE_DIR.
//
// Phase 13 ships cassette-only dispatch in the PHP target. Live
// providers (OpenAI/Anthropic/Google/llama.cpp/...) are deferred; the
// helper emits a stderr diagnostic and returns "" when the env var
// is unset, mirroring the C runtime's behaviour without libcurl.
func TestPhase13EmitFragments(t *testing.T) {
	cases := []struct {
		fixture string // <name>/<name>.mochi
		wants   []string
	}{
		{
			fixture: "generate_text",
			wants: []string{
				// Inline runtime ships with every LLM-using program.
				`function mochi_llm_cassette_key(string $provider, string $model, string $prompt): string`,
				`function mochi_llm_generate(string $provider, string $model, string $prompt): string`,
				// DJB2 hash math runs in GMP because the uint64 result
				// can exceed PHP_INT_MAX (some real cassette ids do).
				`$h = gmp_init(5381);`,
				`$mask = gmp_init('FFFFFFFFFFFFFFFF', 16);`,
				`$h = gmp_and(gmp_mul($h, 33), $mask);`,
				// Cassette path is `<dir>/<djb2>.txt`. Missing env
				// returns "" with a stderr note (live mode is the
				// next phase).
				`$dir = getenv('MOCHI_LLM_CASSETTE_DIR');`,
				`$path = rtrim($dir, '/') . '/' . $key . '.txt';`,
				`$data = @file_get_contents($path);`,
				// User call-site: provider literal flows in as a
				// string arg; empty model means provider default.
				`$r = mochi_llm_generate("openai", "", "Say hello.");`,
			},
		},
		{
			fixture: "generate_anthropic",
			wants: []string{
				// Provider name changes; helper signature is uniform.
				`$r = mochi_llm_generate("anthropic", "", "Count to 3.");`,
			},
		},
		{
			fixture: "generate_concat",
			wants: []string{
				// Awaited string flows into a plain `.` concat at the
				// call site (no special handling needed).
				`$r = mochi_llm_generate("openai", "", "Capital of France?");`,
			},
		},
		{
			fixture: "generate_confirm",
			wants: []string{
				`$r = mochi_llm_generate("anthropic", "", "Reply with only the word: yes");`,
			},
		},
		{
			fixture: "generate_in_var",
			wants: []string{
				// The result of generate flows into a let binding,
				// then a string concat, then print; no LLM-specific
				// wrapper is needed beyond the helper call.
				`$r = mochi_llm_generate("openai", "", "What color is the sky?");`,
			},
		},
		{
			fixture: "generate_math",
			wants: []string{
				`$r = mochi_llm_generate("openai", "", "What is 6 times 7?");`,
			},
		},
		{
			fixture: "generate_multiple",
			wants: []string{
				// Two sequential generate calls in one program; each
				// lowers to a separate helper call, results bound to
				// separate variables.
				`$a = mochi_llm_generate("openai", "", "Say foo.");`,
				`$b = mochi_llm_generate("openai", "", "Is Mochi great?");`,
			},
		},
		{
			fixture: "generate_prime",
			wants: []string{
				`$r = mochi_llm_generate("openai", "", "Is 7 prime?");`,
			},
		},
	}

	for _, c := range cases {
		t.Run(c.fixture, func(t *testing.T) {
			mochiPath := filepath.Join(repoRoot(t), "tests", "transpiler3", "php", "fixtures", "phase13-llm", c.fixture, c.fixture+".mochi")
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
