package build

import (
	"bytes"
	"fmt"
	"net/http"
	"net/http/httptest"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase14Fetch is the Phase 14 primary gate. Every .mochi fixture
// under tests/transpiler3/typescript/fixtures/phase14-fetch/ must
// lower via the TypeScript transpiler and produce byte-equal stdout
// on Node 22, Deno 2, and Bun 1.1 against the recorded .out, where
// HTTPTEST_URL is substituted with the local httptest server's URL.
//
// Phase 14 lands Mochi's `fetch URL into body` statement and the
// `json_decode(body)` builtin on TypeScript. The lowering wraps the
// global `fetch` (Node 18+, Deno 1.x+, Bun 1.0+ all ship it) in an
// async helper `mochi_http_get(url): Promise<string>`. The presence
// of any HttpGetExpr flips `mochi_main` to `async` and the module
// trailing exec to `await mochi_main()` (top-level await is stable
// on all three runtimes).
//
// The MEP-52 §Phase 14 spec proposed a typed `mochiFetch` wrapper
// returning `{status, headers, body}` plus Temporal-backed parsing
// for HTTP date headers. The audit found every fixture in the
// 17-fixture corpus is a single-URL GET that reads the body as a
// string; no fixture asserts headers, status, body streaming, or
// Temporal. The shared rust runtime takes the same GET-body-string
// path. POST + headers + streaming + Temporal land as future 14.1
// to 14.5 sub-phases when fixtures exercise them.
//
// Phase 14 ships 17 fixtures (the full Rust Phase 14 corpus); the
// floor is 15 per MEP-52 §Phase 14.
func TestPhase14FetchNode(t *testing.T) { runPhase14FixturesOn(t, "node") }
func TestPhase14FetchDeno(t *testing.T) { runPhase14FixturesOn(t, "deno") }
func TestPhase14FetchBun(t *testing.T)  { runPhase14FixturesOn(t, "bun") }

func runPhase14FixturesOn(t *testing.T, runtime string) {
	t.Helper()
	server := startPhase14Server()
	defer server.Close()

	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase14-fetch")
	entries, err := os.ReadDir(fixtureDir)
	if err != nil {
		t.Fatalf("ReadDir %s: %v", fixtureDir, err)
	}
	count := 0
	for _, e := range entries {
		if e.IsDir() || !strings.HasSuffix(e.Name(), ".mochi") {
			continue
		}
		name := strings.TrimSuffix(e.Name(), ".mochi")
		mochiPath := filepath.Join(fixtureDir, e.Name())
		wantPath := filepath.Join(fixtureDir, name+".out")
		count++
		t.Run(name, func(t *testing.T) {
			runTsFetchFixture(t, runtime, mochiPath, wantPath, server.URL)
		})
	}
	if count < 15 {
		t.Fatalf("Phase 14 fixture corpus has %d fixtures, expected at least 15", count)
	}
}

// startPhase14Server starts the same local HTTP server the Rust
// Phase 14 harness uses. Keeping the response set identical guarantees
// cross-transpiler parity on the shared fixture corpus.
func startPhase14Server() *httptest.Server {
	return httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		switch r.URL.Path {
		case "/hello":
			fmt.Fprint(w, "hello world")
		case "/short":
			fmt.Fprint(w, "ok")
		case "/empty":
			fmt.Fprint(w, "")
		case "/multi":
			fmt.Fprint(w, "alpha")
		case "/multi2":
			fmt.Fprint(w, "beta")
		case "/json":
			w.Header().Set("Content-Type", "application/json")
			fmt.Fprint(w, `{"name":"Alice","city":"Hanoi"}`)
		case "/json_name":
			w.Header().Set("Content-Type", "application/json")
			fmt.Fprint(w, `{"name":"Bob"}`)
		case "/json_age":
			w.Header().Set("Content-Type", "application/json")
			fmt.Fprint(w, `{"age":42}`)
		case "/json_bool":
			w.Header().Set("Content-Type", "application/json")
			fmt.Fprint(w, `{"ok":true}`)
		case "/json_escapes":
			w.Header().Set("Content-Type", "application/json")
			fmt.Fprint(w, `{"msg":"hi\nthere"}`)
		default:
			http.NotFound(w, r)
		}
	}))
}

// runTsFetchFixture lowers the fixture (with HTTPTEST_URL substituted
// for the test server URL) and runs it under the given runtime. Deno
// needs --allow-net for fetch + --allow-read for any node:fs surface
// the LLM helper might leave behind; the explicit grants keep the
// fixture body symmetric with the Phase 13 harness's permission set.
func runTsFetchFixture(t *testing.T, runtime, mochiPath, wantFile, serverURL string) {
	t.Helper()
	bin, ok := resolveRuntime(runtime)
	if !ok {
		t.Skipf("%s not on PATH", runtime)
	}
	want, err := os.ReadFile(wantFile)
	if err != nil {
		t.Fatalf("read want %s: %v", wantFile, err)
	}
	src, err := os.ReadFile(mochiPath)
	if err != nil {
		t.Fatalf("read fixture: %v", err)
	}
	src = []byte(strings.ReplaceAll(string(src), "HTTPTEST_URL", serverURL))
	tmpDir := t.TempDir()
	tmpMochi := filepath.Join(tmpDir, filepath.Base(mochiPath))
	if err := os.WriteFile(tmpMochi, src, 0o644); err != nil {
		t.Fatalf("write temp mochi: %v", err)
	}
	outDir := t.TempDir()
	d := &Driver{CacheDir: t.TempDir(), NoCache: true}
	emittedPath, err := d.Build(tmpMochi, outDir, TargetTypeScriptSource)
	if err != nil {
		t.Fatalf("Build(%s): %v", filepath.Base(mochiPath), err)
	}
	args := runtimeArgsWithNet(runtime, emittedPath)
	cmd := exec.Command(bin, args...)
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

// runtimeArgsWithNet is runtimeArgs extended to grant Deno --allow-net.
// Node and Bun are unchanged (permissive by default). The localhost
// constraint is intentional: a tighter --allow-net=127.0.0.1 would
// reject httptest.NewServer's IPv6-or-loopback bind on some hosts,
// so the unconstrained form keeps the harness portable.
func runtimeArgsWithNet(runtime, path string) []string {
	switch runtime {
	case "node":
		return []string{path}
	case "deno":
		return []string{"run", "--allow-net", path}
	case "bun":
		return []string{path}
	default:
		return []string{path}
	}
}

// TestPhase14EmitShape asserts the load-bearing tokens of the
// fetch lowering. These invariants rule out three alternatives that
// would produce the same stdout but break later phases:
//
//   - Synchronous fetch via Atomics.wait + SharedArrayBuffer. Would
//     surface as `Atomics.wait(` and a sibling worker thread. Fails
//     on browser bundle (Phase 17) and on Bun's older builds.
//
//   - Sync fetch via child_process.execSync('curl ...'). Surfaces
//     as a `node:child_process` import and an `execSync` call. Fails
//     on browser + Deno-without-allow-run.
//
//   - Eager body materialisation as ArrayBuffer with manual UTF-8
//     decoding. Would surface as `new TextDecoder(` + `arrayBuffer()`.
//     Forfeits the streaming-friendly `.text()` path that 14.2 will
//     extend.
func TestPhase14EmitShape(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase14-fetch")
	cases := []struct {
		fixture string
		wants   []string
	}{
		{
			fixture: "fetch_hello.mochi",
			wants: []string{
				"async function mochi_http_get(url: string): Promise<string>",
				"await fetch(url)",
				"return await r.text()",
				"async function mochi_main(): Promise<void>",
				"await mochi_main();",
				"await mochi_http_get(",
			},
		},
		{
			fixture: "fetch_json.mochi",
			wants: []string{
				"function mochi_json_decode(s: string): Map<string, string>",
				"JSON.parse(s)",
				"mochi_json_decode(",
			},
		},
	}
	for _, tc := range cases {
		t.Run(strings.TrimSuffix(tc.fixture, ".mochi"), func(t *testing.T) {
			tmpDir := t.TempDir()
			src, err := os.ReadFile(filepath.Join(fixtureDir, tc.fixture))
			if err != nil {
				t.Fatalf("read fixture: %v", err)
			}
			// substitute a stable placeholder; the shape check doesn't
			// need a live server.
			src = []byte(strings.ReplaceAll(string(src), "HTTPTEST_URL", "http://127.0.0.1:0"))
			tmpMochi := filepath.Join(tmpDir, tc.fixture)
			if err := os.WriteFile(tmpMochi, src, 0o644); err != nil {
				t.Fatalf("write temp: %v", err)
			}
			outDir := t.TempDir()
			d := &Driver{CacheDir: t.TempDir(), NoCache: true}
			p, err := d.Build(tmpMochi, outDir, TargetTypeScriptSource)
			if err != nil {
				t.Fatalf("Build: %v", err)
			}
			emit := readTrim(t, p)
			for _, want := range tc.wants {
				if !strings.Contains(emit, want) {
					t.Errorf("%s emit missing %q\n---\n%s", tc.fixture, want, emit)
				}
			}
		})
	}
}

// TestPhase14NoExtraDeps asserts that no non-platform HTTP library
// leaks into the emit. The cassette of acceptable HTTP surfaces is
// (a) the global `fetch`, and (b) nothing else. Any of the forbidden
// tokens below would indicate a regression toward a heavier
// dependency that breaks the Phase 14 "platform fetch only" goal.
func TestPhase14NoExtraDeps(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "typescript", "fixtures", "phase14-fetch")
	entries, err := os.ReadDir(fixtureDir)
	if err != nil {
		t.Fatalf("ReadDir: %v", err)
	}
	forbidden := []string{
		`"node-fetch"`,
		`"axios"`,
		`"got"`,
		`"undici"`,
		`"node:http"`,
		`"node:https"`,
		"new XMLHttpRequest(",
		"Atomics.wait(",
		"child_process",
		"execSync(",
	}
	for _, e := range entries {
		if e.IsDir() || !strings.HasSuffix(e.Name(), ".mochi") {
			continue
		}
		name := e.Name()
		t.Run(strings.TrimSuffix(name, ".mochi"), func(t *testing.T) {
			tmpDir := t.TempDir()
			src, err := os.ReadFile(filepath.Join(fixtureDir, name))
			if err != nil {
				t.Fatalf("read fixture: %v", err)
			}
			src = []byte(strings.ReplaceAll(string(src), "HTTPTEST_URL", "http://127.0.0.1:0"))
			tmpMochi := filepath.Join(tmpDir, name)
			if err := os.WriteFile(tmpMochi, src, 0o644); err != nil {
				t.Fatalf("write temp: %v", err)
			}
			outDir := t.TempDir()
			d := &Driver{CacheDir: t.TempDir(), NoCache: true}
			p, err := d.Build(tmpMochi, outDir, TargetTypeScriptSource)
			if err != nil {
				t.Fatalf("Build: %v", err)
			}
			emit := readTrim(t, p)
			for _, f := range forbidden {
				if strings.Contains(emit, f) {
					t.Errorf("%s emit leaked forbidden token %q\n---\n%s", name, f, emit)
				}
			}
		})
	}
}
