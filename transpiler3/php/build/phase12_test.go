package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase12FFI walks the Phase 12 file-I/O fixtures and exercises
// each through the full PHP transpiler. Tests skip when PHP is not
// installed; CI uses shivammathur/setup-php@v2 to drive the end-to-end
// gate.
func TestPhase12FFI(t *testing.T) {
	fixtureDir := filepath.Join(repoRoot(t), "tests", "transpiler3", "php", "fixtures", "phase12-ffi")
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

// TestPhase12EmitFragments asserts that the PHP lowerer maps the
// Mochi file-I/O builtins to PHP's standard library:
//   writeFile  -> file_put_contents($path, $content)
//   appendFile -> file_put_contents($path, $content, FILE_APPEND)
//   readFile   -> file_get_contents($path)
//   lines      -> file($path, FILE_IGNORE_NEW_LINES)
//
// These are the cleanest one-to-one mappings PHP provides; no
// runtime helpers are needed.
func TestPhase12EmitFragments(t *testing.T) {
	cases := []struct {
		fixture string
		wants   []string
	}{
		{
			fixture: "ffi_write_read.mochi",
			wants: []string{
				`file_put_contents("/tmp/mochi_swift_wr.txt", "hello world");`,
				`$content = file_get_contents("/tmp/mochi_swift_wr.txt");`,
				`mochi_print_str($content);`,
			},
		},
		{
			fixture: "ffi_append.mochi",
			wants: []string{
				// First write truncates; second call passes
				// FILE_APPEND so the bytes accumulate rather than
				// overwrite.
				`file_put_contents("/tmp/mochi_swift_app.txt", "hello");`,
				`file_put_contents("/tmp/mochi_swift_app.txt", " world", FILE_APPEND);`,
				`$content = file_get_contents("/tmp/mochi_swift_app.txt");`,
			},
		},
		{
			fixture: "ffi_append_multiple.mochi",
			wants: []string{
				// Three sequential appends produce three
				// file_put_contents calls; each carries FILE_APPEND so
				// none of them truncates.
				`file_put_contents("/tmp/mochi_swift_am.txt", "a");`,
				`file_put_contents("/tmp/mochi_swift_am.txt", "b", FILE_APPEND);`,
				`file_put_contents("/tmp/mochi_swift_am.txt", "c", FILE_APPEND);`,
			},
		},
		{
			fixture: "ffi_lines.mochi",
			wants: []string{
				// `lines(path)` uses FILE_IGNORE_NEW_LINES so each
				// entry has its trailing newline stripped, and a
				// trailing newline at EOF does not produce an empty
				// last element.
				`$xs = file("/tmp/mochi_swift_lines.txt", FILE_IGNORE_NEW_LINES);`,
				`foreach ($xs as $line)`,
			},
		},
		{
			fixture: "ffi_lines_count.mochi",
			wants: []string{
				// count() over the file()-returned array gives the
				// line count; len() lowers to count() in the PHP
				// target's list path.
				`$xs = file("/tmp/mochi_swift_lc.txt", FILE_IGNORE_NEW_LINES);`,
				`count($xs)`,
			},
		},
		{
			fixture: "ffi_lines_for.mochi",
			wants: []string{
				// foreach over the lines() result iterates each
				// stripped line in order.
				`$xs = file("/tmp/mochi_swift_lf.txt", FILE_IGNORE_NEW_LINES);`,
				`foreach ($xs as $line)`,
				`mochi_print_str($line);`,
			},
		},
		{
			fixture: "ffi_multiple_files.mochi",
			wants: []string{
				// Two distinct paths in one program: each write+read
				// pair stays independent at the PHP source level.
				`file_put_contents("/tmp/mochi_swift_mf1.txt", "file one");`,
				`file_put_contents("/tmp/mochi_swift_mf2.txt", "file two");`,
				`$a = file_get_contents("/tmp/mochi_swift_mf1.txt");`,
				`$b = file_get_contents("/tmp/mochi_swift_mf2.txt");`,
			},
		},
		{
			fixture: "ffi_newlines.mochi",
			wants: []string{
				// Embedded `\n` inside the literal stays as `\n` in
				// PHP source; PHP's double-quoted string interprets it
				// as the newline byte, matching Mochi's semantic.
				`file_put_contents("/tmp/mochi_swift_nl.txt", "line1\nline2\nline3");`,
				`$content = file_get_contents("/tmp/mochi_swift_nl.txt");`,
			},
		},
		{
			fixture: "ffi_overwrite.mochi",
			wants: []string{
				// Second writeFile on the same path uses default flags
				// (no FILE_APPEND), so PHP truncates and replaces the
				// existing contents.
				`file_put_contents("/tmp/mochi_swift_ow.txt", "first");`,
				`file_put_contents("/tmp/mochi_swift_ow.txt", "second");`,
			},
		},
		{
			fixture: "ffi_overwrite_check.mochi",
			wants: []string{
				`file_put_contents("/tmp/mochi_swift_oc.txt", "original");`,
				`file_put_contents("/tmp/mochi_swift_oc.txt", "replaced");`,
				`$content = file_get_contents("/tmp/mochi_swift_oc.txt");`,
			},
		},
	}

	for _, c := range cases {
		t.Run(strings.TrimSuffix(c.fixture, ".mochi"), func(t *testing.T) {
			mochiPath := filepath.Join(repoRoot(t), "tests", "transpiler3", "php", "fixtures", "phase12-ffi", c.fixture)
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
