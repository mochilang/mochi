package build

import (
	"archive/tar"
	"archive/zip"
	"bytes"
	"compress/gzip"
	"io"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"testing"
	"time"
)

// TestPhase16Reproducible is the gate for Phase 16. Phase 15.0
// already ships byte-deterministic wheels (mtime pinned, sorted
// paths); Phase 16 verifies the supply-chain contract end-to-end:
//
//   - wheel and sdist SHA-256 are byte-equal across rebuilds
//   - SOURCE_DATE_EPOCH overrides the 1980 epoch floor
//   - the source emit is a fixed point (emit twice; byte-equal)
//   - the wheel RECORD entries are lex-sorted (PEP 376)
//   - the wheel contains no __pycache__ / *.pyc entries
//   - the gzip header inside the sdist has a pinned ModTime
//
// The corpus is two dedicated fixtures (reproducibility_basic +
// reproducibility_with_extras) plus the Phase 1 hello fixture as a
// regression anchor. Cross-host SHA-match runs in the dedicated CI
// matrix (.github/workflows/transpiler3-python-reproducibility.yml).
func TestPhase16Reproducible(t *testing.T) {
	fixtureRoot := filepath.Join(repoRootForBuild(t), "tests", "transpiler3", "python", "fixtures", "phase16-repro")
	fixtures := []string{
		filepath.Join(fixtureRoot, "reproducibility_basic", "repro_basic.mochi"),
		filepath.Join(fixtureRoot, "reproducibility_with_extras", "repro_extras.mochi"),
	}

	t.Run("wheel_sha_byte_equal_across_rebuilds", func(t *testing.T) {
		for _, fx := range fixtures {
			fx := fx
			t.Run(filepath.Base(filepath.Dir(fx)), func(t *testing.T) {
				out1 := t.TempDir()
				out2 := t.TempDir()
				d1 := &Driver{CacheDir: t.TempDir()}
				d2 := &Driver{CacheDir: t.TempDir()}
				if err := d1.Build(fx, out1, TargetPythonWheel); err != nil {
					t.Fatalf("build 1: %v", err)
				}
				if err := d2.Build(fx, out2, TargetPythonWheel); err != nil {
					t.Fatalf("build 2: %v", err)
				}
				w1 := filepath.Join(out1, d1.packageName(fx)+"-"+distVersion+"-py3-none-any.whl")
				w2 := filepath.Join(out2, d2.packageName(fx)+"-"+distVersion+"-py3-none-any.whl")
				s1, err := sha256File(w1)
				if err != nil {
					t.Fatalf("sha256 1: %v", err)
				}
				s2, err := sha256File(w2)
				if err != nil {
					t.Fatalf("sha256 2: %v", err)
				}
				if s1 != s2 {
					t.Errorf("wheel SHA-256 diverged across rebuilds: %s vs %s", s1, s2)
				}
			})
		}
	})

	t.Run("sdist_sha_byte_equal_across_rebuilds", func(t *testing.T) {
		for _, fx := range fixtures {
			fx := fx
			t.Run(filepath.Base(filepath.Dir(fx)), func(t *testing.T) {
				out1 := t.TempDir()
				out2 := t.TempDir()
				d1 := &Driver{CacheDir: t.TempDir()}
				d2 := &Driver{CacheDir: t.TempDir()}
				if err := d1.Build(fx, out1, TargetPythonSdist); err != nil {
					t.Fatalf("build 1: %v", err)
				}
				if err := d2.Build(fx, out2, TargetPythonSdist); err != nil {
					t.Fatalf("build 2: %v", err)
				}
				s1, err := sha256File(filepath.Join(out1, d1.packageName(fx)+"-"+distVersion+".tar.gz"))
				if err != nil {
					t.Fatalf("sha256 1: %v", err)
				}
				s2, err := sha256File(filepath.Join(out2, d2.packageName(fx)+"-"+distVersion+".tar.gz"))
				if err != nil {
					t.Fatalf("sha256 2: %v", err)
				}
				if s1 != s2 {
					t.Errorf("sdist SHA-256 diverged across rebuilds: %s vs %s", s1, s2)
				}
			})
		}
	})

	t.Run("source_emit_fixed_point", func(t *testing.T) {
		// Two source builds of the same fixture produce byte-equal
		// generated/<module>.py files. Catches any non-deterministic
		// ordering in the Python lower (e.g. map iteration leaking
		// into emit order).
		for _, fx := range fixtures {
			fx := fx
			t.Run(filepath.Base(filepath.Dir(fx)), func(t *testing.T) {
				out1 := t.TempDir()
				out2 := t.TempDir()
				d1 := &Driver{CacheDir: t.TempDir()}
				d2 := &Driver{CacheDir: t.TempDir()}
				if err := d1.Build(fx, out1, TargetPythonSource); err != nil {
					t.Fatalf("build 1: %v", err)
				}
				if err := d2.Build(fx, out2, TargetPythonSource); err != nil {
					t.Fatalf("build 2: %v", err)
				}
				pkgName := d1.packageName(fx)
				moduleBase := strings.TrimSuffix(filepath.Base(fx), ".mochi")
				gen1 := filepath.Join(out1, "src", pkgName, "generated", moduleBase+".py")
				gen2 := filepath.Join(out2, "src", pkgName, "generated", moduleBase+".py")
				b1, err := os.ReadFile(gen1)
				if err != nil {
					t.Fatalf("read gen1: %v", err)
				}
				b2, err := os.ReadFile(gen2)
				if err != nil {
					t.Fatalf("read gen2: %v", err)
				}
				if !bytes.Equal(b1, b2) {
					t.Errorf("generated source diverged on rebuild (%d vs %d bytes)", len(b1), len(b2))
				}
			})
		}
	})

	t.Run("wheel_record_is_lex_sorted", func(t *testing.T) {
		// PEP 376 does not strictly require lex-sorted RECORD lines
		// but every reproducible-build tooling (hatchling, flit,
		// pip wheel) emits them sorted; the Mochi builder must match.
		out := t.TempDir()
		d := &Driver{CacheDir: t.TempDir()}
		fx := fixtures[1]
		if err := d.Build(fx, out, TargetPythonWheel); err != nil {
			t.Fatalf("build: %v", err)
		}
		pkgName := d.packageName(fx)
		wheelPath := filepath.Join(out, pkgName+"-"+distVersion+"-py3-none-any.whl")
		recordPath := pkgName + "-" + distVersion + ".dist-info/RECORD"
		recordBytes, err := readZipEntry(wheelPath, recordPath)
		if err != nil {
			t.Fatalf("read RECORD: %v", err)
		}
		var paths []string
		for _, line := range strings.Split(string(recordBytes), "\n") {
			if line == "" {
				continue
			}
			i := strings.IndexByte(line, ',')
			if i < 0 {
				t.Fatalf("malformed RECORD line: %q", line)
			}
			paths = append(paths, line[:i])
		}
		sorted := append([]string(nil), paths...)
		sort.Strings(sorted)
		if !stringSliceEqual(paths, sorted) {
			t.Errorf("RECORD entries not lex-sorted; got %v want %v", paths, sorted)
		}
	})

	t.Run("wheel_has_no_pycache", func(t *testing.T) {
		// pip / hatchling never ship .pyc; the Phase 15 builder
		// applies pyOnlyFilter to the runtime copy. Anything that
		// leaked through here would also defeat reproducibility
		// (pyc headers include the build host's Python version +
		// mtime nanoseconds).
		out := t.TempDir()
		d := &Driver{CacheDir: t.TempDir()}
		fx := fixtures[1]
		if err := d.Build(fx, out, TargetPythonWheel); err != nil {
			t.Fatalf("build: %v", err)
		}
		pkgName := d.packageName(fx)
		wheelPath := filepath.Join(out, pkgName+"-"+distVersion+"-py3-none-any.whl")
		entries, err := listZipEntries(wheelPath)
		if err != nil {
			t.Fatalf("list zip: %v", err)
		}
		for _, name := range entries {
			if strings.Contains(name, "__pycache__/") || strings.HasSuffix(name, ".pyc") {
				t.Errorf("wheel contains build-host bytecode: %q", name)
			}
		}
	})

	t.Run("source_date_epoch_overrides_floor", func(t *testing.T) {
		// SOURCE_DATE_EPOCH at a post-1980 instant must flow into
		// the zip entry mtimes and the gzip header. This is the
		// reproducible-builds.org contract for downstream tooling
		// that compares wheels to source provenance.
		const epoch = "1700000000" // 2023-11-14T22:13:20Z
		t.Setenv("SOURCE_DATE_EPOCH", epoch)
		out := t.TempDir()
		d := &Driver{CacheDir: t.TempDir()}
		fx := fixtures[0]
		if err := d.Build(fx, out, TargetPythonWheel); err != nil {
			t.Fatalf("build wheel: %v", err)
		}
		pkgName := d.packageName(fx)
		wheelPath := filepath.Join(out, pkgName+"-"+distVersion+"-py3-none-any.whl")
		mtimes, err := zipEntryMTimes(wheelPath)
		if err != nil {
			t.Fatalf("read mtimes: %v", err)
		}
		want := time.Unix(1700000000, 0).UTC()
		for name, got := range mtimes {
			// Zip stores mtime in DOS format with 2-second
			// resolution; round both sides to the nearest 2 seconds.
			if absDur(got.Sub(want)) > 2*time.Second {
				t.Errorf("wheel entry %s mtime = %v; want %v (SOURCE_DATE_EPOCH=%s)", name, got, want, epoch)
			}
		}

		if err := d.Build(fx, out, TargetPythonSdist); err != nil {
			t.Fatalf("build sdist: %v", err)
		}
		sdistPath := filepath.Join(out, pkgName+"-"+distVersion+".tar.gz")
		gzMTime, err := readGzipHeaderMTime(sdistPath)
		if err != nil {
			t.Fatalf("read gzip mtime: %v", err)
		}
		if !gzMTime.Equal(want) {
			t.Errorf("sdist gzip header ModTime = %v; want %v (SOURCE_DATE_EPOCH=%s)", gzMTime, want, epoch)
		}
	})

	t.Run("source_date_epoch_falls_back_when_malformed", func(t *testing.T) {
		// Belt-and-braces: a malformed value (negative or non-int)
		// must not crash the build; falls back to the 1980 floor.
		for _, bad := range []string{"-1", "not-a-number", "9999999999999999999999"} {
			bad := bad
			t.Run(bad, func(t *testing.T) {
				t.Setenv("SOURCE_DATE_EPOCH", bad)
				got := sourceDateEpoch()
				floor := time.Date(1980, 1, 1, 0, 0, 0, 0, time.UTC)
				if !got.Equal(floor) {
					t.Errorf("sourceDateEpoch(%q) = %v; want floor %v", bad, got, floor)
				}
			})
		}
	})
}

func readZipEntry(zipPath, name string) ([]byte, error) {
	r, err := zip.OpenReader(zipPath)
	if err != nil {
		return nil, err
	}
	defer r.Close()
	for _, f := range r.File {
		if f.Name == name {
			rc, err := f.Open()
			if err != nil {
				return nil, err
			}
			defer rc.Close()
			return io.ReadAll(rc)
		}
	}
	return nil, os.ErrNotExist
}

func listZipEntries(zipPath string) ([]string, error) {
	r, err := zip.OpenReader(zipPath)
	if err != nil {
		return nil, err
	}
	defer r.Close()
	var names []string
	for _, f := range r.File {
		names = append(names, f.Name)
	}
	return names, nil
}

func zipEntryMTimes(zipPath string) (map[string]time.Time, error) {
	r, err := zip.OpenReader(zipPath)
	if err != nil {
		return nil, err
	}
	defer r.Close()
	out := map[string]time.Time{}
	for _, f := range r.File {
		out[f.Name] = f.Modified.UTC()
	}
	return out, nil
}

func readGzipHeaderMTime(path string) (time.Time, error) {
	f, err := os.Open(path)
	if err != nil {
		return time.Time{}, err
	}
	defer f.Close()
	gz, err := gzip.NewReader(f)
	if err != nil {
		return time.Time{}, err
	}
	defer gz.Close()
	return gz.ModTime.UTC(), nil
}

func stringSliceEqual(a, b []string) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

func absDur(d time.Duration) time.Duration {
	if d < 0 {
		return -d
	}
	return d
}

// Compile-time use to silence the unused import linter for tar in
// builds that don't run the sdist gate path. (tar is used by the
// archive/tar reader within Phase 15 helpers but the Phase 16 test
// uses gzip directly for the header check.)
var _ = tar.TypeReg
