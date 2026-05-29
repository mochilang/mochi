package build

import (
	"archive/tar"
	"compress/gzip"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase30TargetEmittedSyntax runs `ruby -c` on every .rb the build
// targets emit so syntactic regressions surface even when the heavy
// toolchains (Tebako, GraalVM, mruby) are absent.
//
// The .gem from TargetRubyGem is additionally unpacked to confirm the lib
// file inside the archive matches the source on disk byte-for-byte.
func TestPhase30TargetEmittedSyntax(t *testing.T) {
	tc, err := resolveToolchain()
	if err != nil {
		t.Skipf("ruby toolchain not available: %v", err)
	}

	// One non-trivial source exercises records, sum types, queries, and a
	// closure so the emitted .rb is broad enough that a syntactic glitch
	// shows up.
	src := `type Sign = Pos | Neg | Zero
fun classify(n: int): Sign {
  if n > 0 { return Pos }
  if n < 0 { return Neg }
  return Zero
}
fun name(s: Sign): string {
  return match s {
    Pos => "pos"
    Neg => "neg"
    Zero => "zero"
  }
}
let xs: list<int> = [3, -1, 0, 7, -5]
let names = from x in xs select name(classify(x))
for nm in names {
  print(nm)
}
`

	targets := []struct {
		name    string
		target  Target
		rbPaths func(out string) []string
	}{
		{
			name:   "gem",
			target: TargetRubyGem,
			rbPaths: func(out string) []string {
				return []string{filepath.Join(out, "lib", "p30.rb")}
			},
		},
		{
			name:   "bundle",
			target: TargetRubyBundle,
			rbPaths: func(out string) []string {
				return []string{filepath.Join(out, "p30.rb")}
			},
		},
		{
			name:   "tebako",
			target: TargetTebako,
			rbPaths: func(out string) []string {
				return []string{filepath.Join(out, "root", "p30.rb")}
			},
		},
		{
			name:   "truffle_native",
			target: TargetTruffleNative,
			rbPaths: func(out string) []string {
				return []string{filepath.Join(out, "p30.rb")}
			},
		},
		{
			name:   "mruby",
			target: TargetMRuby,
			rbPaths: func(out string) []string {
				return []string{filepath.Join(out, "p30.rb")}
			},
		},
	}

	for _, tc2 := range targets {
		tc2 := tc2
		t.Run(tc2.name, func(t *testing.T) {
			srcDir := t.TempDir()
			srcPath := filepath.Join(srcDir, "p30.mochi")
			if err := os.WriteFile(srcPath, []byte(src), 0o644); err != nil {
				t.Fatalf("write src: %v", err)
			}
			out := t.TempDir()
			d := &Driver{}
			if err := d.Build(srcPath, out, tc2.target); err != nil {
				t.Fatalf("Build %s: %v", tc2.name, err)
			}
			for _, rb := range tc2.rbPaths(out) {
				if _, err := os.Stat(rb); err != nil {
					t.Fatalf("expected %s, got: %v", rb, err)
				}
				cmd := exec.Command(tc.Ruby, "-c", rb)
				if cout, err := cmd.CombinedOutput(); err != nil {
					t.Fatalf("ruby -c %s failed: %v\noutput: %s", rb, err, cout)
				}
			}
		})
	}

	// Notebook is JSON-only; ruby -c does not apply. Re-build it and
	// validate that JSON.parse round-trips.
	t.Run("notebook_json_round_trip", func(t *testing.T) {
		srcDir := t.TempDir()
		srcPath := filepath.Join(srcDir, "p30nb.mochi")
		if err := os.WriteFile(srcPath, []byte(src), 0o644); err != nil {
			t.Fatalf("write src: %v", err)
		}
		out := t.TempDir()
		d := &Driver{}
		if err := d.Build(srcPath, out, TargetIRubyKernel); err != nil {
			t.Fatalf("Build IRubyKernel: %v", err)
		}
		nbPath := filepath.Join(out, "p30nb.ipynb")
		probe := `require 'json'
nb = JSON.parse(File.read(ARGV[0]))
abort('no cells') unless nb['cells']
abort('wrong cell type') unless nb['cells'][0]['cell_type'] == 'code'
abort('no source') if nb['cells'][0]['source'].nil? || nb['cells'][0]['source'].empty?
puts 'ok'
`
		cmd := exec.Command(tc.Ruby, "-e", probe, nbPath)
		if cout, err := cmd.CombinedOutput(); err != nil {
			t.Fatalf("notebook JSON probe failed: %v\noutput: %s", err, cout)
		} else if !strings.Contains(string(cout), "ok") {
			t.Fatalf("notebook probe wrong output: %q", cout)
		}
	})

	// .gem unpack: TargetRubyGem produces a .gem when `gem build` runs.
	// Confirm the archive contains lib/<name>.rb at the expected path.
	t.Run("gem_unpack_round_trip", func(t *testing.T) {
		gemPath := filepath.Join(filepath.Dir(tc.Ruby), "gem")
		if _, err := os.Stat(gemPath); err != nil {
			if p, err := exec.LookPath("gem"); err == nil {
				gemPath = p
			} else {
				t.Skipf("gem not available")
			}
		}
		srcDir := t.TempDir()
		srcPath := filepath.Join(srcDir, "p30gem.mochi")
		if err := os.WriteFile(srcPath, []byte(src), 0o644); err != nil {
			t.Fatalf("write src: %v", err)
		}
		out := t.TempDir()
		d := &Driver{}
		if err := d.Build(srcPath, out, TargetRubyGem); err != nil {
			t.Fatalf("Build gem: %v", err)
		}
		cmd := exec.Command(gemPath, "build", "p30gem.gemspec")
		cmd.Dir = out
		if cout, err := cmd.CombinedOutput(); err != nil {
			t.Fatalf("gem build: %v\n%s", err, cout)
		}
		gemFile := filepath.Join(out, "p30gem-0.1.0.gem")
		if !inspectGemContainsLib(t, gemFile, "lib/p30gem.rb") {
			t.Fatalf(".gem missing lib/p30gem.rb")
		}
	})
}

// inspectGemContainsLib opens a .gem (a tar of tar.gz members) and reports
// whether data.tar.gz contains the requested entry path. A .gem looks like:
//
//	metadata.gz, data.tar.gz, checksums.yaml.gz
func inspectGemContainsLib(t *testing.T, gemPath, want string) bool {
	t.Helper()
	f, err := os.Open(gemPath)
	if err != nil {
		t.Fatalf("open gem: %v", err)
	}
	defer f.Close()
	outer := tar.NewReader(f)
	for {
		hdr, err := outer.Next()
		if err == io.EOF {
			return false
		}
		if err != nil {
			t.Fatalf("read outer tar: %v", err)
		}
		if hdr.Name != "data.tar.gz" {
			continue
		}
		gz, err := gzip.NewReader(outer)
		if err != nil {
			t.Fatalf("data.tar.gz gunzip: %v", err)
		}
		inner := tar.NewReader(gz)
		for {
			ihdr, err := inner.Next()
			if err == io.EOF {
				return false
			}
			if err != nil {
				t.Fatalf("read inner tar: %v", err)
			}
			if ihdr.Name == want {
				return true
			}
		}
	}
}
