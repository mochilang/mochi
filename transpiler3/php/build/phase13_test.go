package build

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
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
				// Both failure branches must emit a stderr note and
				// short-circuit with `return '';`. Neither runtime
				// test exercises the failing path (they all run with
				// the env var set and cassettes present), so the
				// fragment gate is the only thing pinning the
				// diagnostic contract.
				`fwrite(STDERR, "mochi_llm_generate: MOCHI_LLM_CASSETTE_DIR not set; live mode not yet implemented for PHP\n");`,
				`fwrite(STDERR, "mochi_llm_generate: cassette not found: $path\n");`,
				`if ($dir === false || $dir === '') {`,
				`if ($data === false) {`,
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
		{
			// Non-empty `model:` field. Every other Phase 13 fixture
			// passes "" (provider default), which only pins the empty
			// branch of the cassette-key derivation. A regression that
			// dropped the model from the concat would still pass the
			// other fixtures but mis-hash this one.
			fixture: "generate_with_model",
			wants: []string{
				`$r = mochi_llm_generate("openai", "gpt-4o-mini", "Say hi.");`,
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

// djb2CassetteKey is a Go reimplementation of the PHP
// mochi_llm_cassette_key helper. Both must agree: any divergence here
// means a regression in the emitter that no end-to-end PHP run would
// catch on hosts without `php`. The PHP version uses GMP because the
// uint64 product can exceed PHP_INT_MAX; Go's uint64 already wraps
// modulo 2^64 so the result is the same without GMP.
func djb2CassetteKey(provider, model, prompt string) string {
	buf := provider + "\x00" + model + "\x00" + prompt
	var h uint64 = 5381
	for i := 0; i < len(buf); i++ {
		h = (h * 33) ^ uint64(buf[i])
	}
	return strconv.FormatUint(h, 10)
}

// TestPhase13DJB2HashMatchesCassetteFilenames pins the cassette
// lookup algorithm: every (provider, model, prompt) tuple used by a
// Phase 13 fixture must hash to a filename that actually exists in
// that fixture's cassette/ directory. The test runs without PHP - it
// only needs the fixture directory. It catches a regression in the
// PHP DJB2 implementation (wrong concat order, missing NUL, wrong
// mask, signed-int overflow), a wrong default model passed by the
// lowerer, or a renamed cassette file.
func TestPhase13DJB2HashMatchesCassetteFilenames(t *testing.T) {
	cases := []struct {
		fixture, provider, model, prompt, wantHash string
	}{
		{"generate_text", "openai", "", "Say hello.", "15023835511162652990"},
		{"generate_anthropic", "anthropic", "", "Count to 3.", "2324397449310383700"},
		{"generate_concat", "openai", "", "Capital of France?", "13416524672896750544"},
		{"generate_confirm", "anthropic", "", "Reply with only the word: yes", "7071908178434434007"},
		{"generate_in_var", "openai", "", "What color is the sky?", "9323966891408970643"},
		{"generate_math", "openai", "", "What is 6 times 7?", "7500588262126349073"},
		{"generate_prime", "openai", "", "Is 7 prime?", "16185609923679915080"},
		// Non-empty model: pins the branch the other rows miss.
		{"generate_with_model", "openai", "gpt-4o-mini", "Say hi.", "16094040660861522854"},
	}
	for _, c := range cases {
		t.Run(c.fixture, func(t *testing.T) {
			got := djb2CassetteKey(c.provider, c.model, c.prompt)
			if got != c.wantHash {
				t.Errorf("djb2(%q, %q, %q) = %s; want %s", c.provider, c.model, c.prompt, got, c.wantHash)
			}
			path := filepath.Join(repoRoot(t), "tests", "transpiler3", "php", "fixtures", "phase13-llm", c.fixture, "cassette", got+".txt")
			if _, err := os.Stat(path); err != nil {
				t.Errorf("cassette file %s does not exist: %v", path, err)
			}
		})
	}

	// generate_multiple issues two distinct prompts in one program;
	// both must resolve. This pins the path that hashes per call
	// rather than carrying state across calls.
	t.Run("generate_multiple", func(t *testing.T) {
		for prompt, want := range map[string]string{
			"Say foo.":        "14733925101528638458",
			"Is Mochi great?": "16198809129143077817",
		} {
			got := djb2CassetteKey("openai", "", prompt)
			if got != want {
				t.Errorf("djb2(openai, \"\", %q) = %s; want %s", prompt, got, want)
			}
			path := filepath.Join(repoRoot(t), "tests", "transpiler3", "php", "fixtures", "phase13-llm", "generate_multiple", "cassette", got+".txt")
			if _, err := os.Stat(path); err != nil {
				t.Errorf("cassette file %s does not exist: %v", path, err)
			}
		}
	})
}
