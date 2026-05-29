package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase14Fetch walks the Phase 14 fetch fixtures and exercises
// each through the full PHP transpiler. Tests skip when PHP is not
// installed; CI uses shivammathur/setup-php@v2 to drive the end-to-end
// gate.
func TestPhase14Fetch(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "php", "fixtures", "phase14-fetch")
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

// TestPhase14EmitFragments asserts that `fetch(url)` lowers to
// `file_get_contents($url)`. PHP's file_get_contents accepts both
// `file://` and `http(s)://` URLs natively through its stream wrappers,
// so a single mapping covers all Phase 14 fixtures (all of which use
// `file://` for offline determinism).
//
// No runtime helpers are needed; the PHP stdlib subsumes the fetch
// semantics directly. If a future fixture exercises HTTP-specific
// behaviour (headers, methods, status codes), a richer helper would
// live alongside the LLM cassette helper from Phase 13.
func TestPhase14EmitFragments(t *testing.T) {
	cases := []struct {
		fixture string
		wants   []string
	}{
		{
			fixture: "fetch_basic.mochi",
			wants: []string{
				// fetch + file:// URL flows through the same stream
				// wrapper as a plain readFile path; the result is the
				// raw bytes of the file.
				`$body = file_get_contents("file:///tmp/mochi_swift_fetch1.txt");`,
				`mochi_print_str($body);`,
			},
		},
		{
			fixture: "fetch_concat.mochi",
			wants: []string{
				// fetched body flows into a `+` concat at the print
				// site, which lowers to PHP's `.` operator.
				`$r = file_get_contents("file:///tmp/mochi_swift_fc.txt");`,
			},
		},
		{
			fixture: "fetch_empty.mochi",
			wants: []string{
				// "empty" here is the fixture name; the cassette
				// contains literal bytes "empty test". No special
				// handling for empty results is needed.
				`$r = file_get_contents("file:///tmp/mochi_swift_fe.txt");`,
			},
		},
		{
			fixture: "fetch_json_string.mochi",
			wants: []string{
				// JSON-shaped payload is just bytes from PHP's
				// perspective; no parser is invoked here.
				`$body = file_get_contents("file:///tmp/mochi_swift_fetch4.txt");`,
			},
		},
		{
			fixture: "fetch_multiline.mochi",
			wants: []string{
				// Embedded \n round-trip: the file_put_contents call
				// writes the literal newline bytes, the fetch reads
				// them back unchanged.
				`file_put_contents("/tmp/mochi_swift_fetch3.txt", "line1\nline2\nline3");`,
				`$body = file_get_contents("file:///tmp/mochi_swift_fetch3.txt");`,
			},
		},
		{
			fixture: "fetch_newlines.mochi",
			wants: []string{
				`$r = file_get_contents("file:///tmp/mochi_swift_fn.txt");`,
			},
		},
		{
			fixture: "fetch_overwrite_fetch.mochi",
			wants: []string{
				// Two writes interleaved with two fetches: each fetch
				// reads the bytes present on disk at that point.
				`file_put_contents("/tmp/mochi_swift_fof.txt", "first");`,
				`$r1 = file_get_contents("file:///tmp/mochi_swift_fof.txt");`,
				`file_put_contents("/tmp/mochi_swift_fof.txt", "second");`,
				`$r2 = file_get_contents("file:///tmp/mochi_swift_fof.txt");`,
			},
		},
		{
			fixture: "fetch_reuse.mochi",
			wants: []string{
				// Same URL fetched twice: both calls reach the same
				// file_get_contents site (PHP doesn't cache stream
				// reads, so r1 and r2 are independent reads).
				`$r1 = file_get_contents("file:///tmp/mochi_swift_fre.txt");`,
				`$r2 = file_get_contents("file:///tmp/mochi_swift_fre.txt");`,
			},
		},
		{
			fixture: "fetch_string.mochi",
			wants: []string{
				// Fetched body bound to a let, then concatenated into
				// a result string before print.
				`$r = file_get_contents("file:///tmp/mochi_swift_fetch2.txt");`,
				`$result = ("Got: " . $r);`,
			},
		},
		{
			fixture: "fetch_use_result.mochi",
			wants: []string{
				// Two fetches plus a three-way `.` concat at print
				// time; each fetched result keeps its own variable.
				`$r1 = file_get_contents("file:///tmp/mochi_swift_fetch5.txt");`,
				`$r2 = file_get_contents("file:///tmp/mochi_swift_fetch5.txt");`,
			},
		},
	}

	for _, c := range cases {
		t.Run(strings.TrimSuffix(c.fixture, ".mochi"), func(t *testing.T) {
			mochiPath := filepath.Join(repoRoot(t), "tests", "transpiler3", "php", "fixtures", "phase14-fetch", c.fixture)
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
