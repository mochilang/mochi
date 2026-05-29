package build

import (
	"archive/tar"
	"archive/zip"
	"bytes"
	"compress/gzip"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"sort"
	"strings"
	"testing"
)

// TestPhase15WheelSdist is the gate for Phase 15.0: TargetPythonWheel
// produces a PEP 427 wheel (`<pkg>-0.1.0-py3-none-any.whl`) and
// TargetPythonSdist produces a PEP 517 sdist
// (`<pkg>-0.1.0.tar.gz`). The wheel must be self-contained
// (bundles `mochi_runtime`), valid (dist-info METADATA + WHEEL +
// RECORD parse), runnable (extracted onto PYTHONPATH the program
// runs and stdout matches), and deterministic (two builds produce
// byte-equal archives).
func TestPhase15WheelSdist(t *testing.T) {
	fixture := filepath.Join(repoRootForBuild(t), "tests", "transpiler3", "python", "fixtures", "phase01-hello", "hello.mochi")
	wantPath := filepath.Join(filepath.Dir(fixture), "hello.out")
	want, err := os.ReadFile(wantPath)
	if err != nil {
		t.Fatalf("read want: %v", err)
	}

	t.Run("wheel_build_runs_and_prints", func(t *testing.T) {
		out := t.TempDir()
		d := &Driver{CacheDir: t.TempDir()}
		if err := d.Build(fixture, out, TargetPythonWheel); err != nil {
			t.Fatalf("Build wheel: %v", err)
		}
		pkgName := d.packageName(fixture)
		wheelPath := filepath.Join(out, pkgName+"-"+distVersion+"-py3-none-any.whl")
		if _, err := os.Stat(wheelPath); err != nil {
			t.Fatalf("expected wheel at %s: %v", wheelPath, err)
		}

		extract := t.TempDir()
		if err := unzipTo(wheelPath, extract); err != nil {
			t.Fatalf("unzip wheel: %v", err)
		}
		for _, want := range []string{
			pkgName + "/__init__.py",
			pkgName + "/__main__.py",
			"mochi_runtime/__init__.py",
			"mochi_runtime/io.py",
			pkgName + "-" + distVersion + ".dist-info/METADATA",
			pkgName + "-" + distVersion + ".dist-info/WHEEL",
			pkgName + "-" + distVersion + ".dist-info/RECORD",
		} {
			if _, err := os.Stat(filepath.Join(extract, want)); err != nil {
				t.Errorf("expected %s in wheel: %v", want, err)
			}
		}

		tc, err := resolveToolchain()
		if err != nil {
			t.Fatalf("resolveToolchain: %v", err)
		}
		cmd := exec.Command(tc.Python, "-m", pkgName)
		cmd.Env = append(os.Environ(),
			"PYTHONPATH="+extract,
			"PYTHONDONTWRITEBYTECODE=1",
		)
		var stdout, stderr bytes.Buffer
		cmd.Stdout = &stdout
		cmd.Stderr = &stderr
		if err := cmd.Run(); err != nil {
			t.Fatalf("python -m %s: %v\nstderr: %s", pkgName, err, stderr.String())
		}
		norm := func(b []byte) []byte { return bytes.ReplaceAll(b, []byte("\r\n"), []byte("\n")) }
		if !bytes.Equal(norm(stdout.Bytes()), norm(want)) {
			t.Errorf("stdout mismatch\ngot:  %q\nwant: %q", stdout.String(), string(want))
		}
	})

	t.Run("sdist_build_contains_pyproject_and_sources", func(t *testing.T) {
		out := t.TempDir()
		d := &Driver{CacheDir: t.TempDir()}
		if err := d.Build(fixture, out, TargetPythonSdist); err != nil {
			t.Fatalf("Build sdist: %v", err)
		}
		pkgName := d.packageName(fixture)
		sdistPath := filepath.Join(out, pkgName+"-"+distVersion+".tar.gz")
		if _, err := os.Stat(sdistPath); err != nil {
			t.Fatalf("expected sdist at %s: %v", sdistPath, err)
		}
		entries, err := tarGzEntries(sdistPath)
		if err != nil {
			t.Fatalf("read sdist: %v", err)
		}
		topdir := pkgName + "-" + distVersion
		for _, want := range []string{
			topdir + "/pyproject.toml",
			topdir + "/PKG-INFO",
			topdir + "/src/" + pkgName + "/__init__.py",
			topdir + "/src/" + pkgName + "/__main__.py",
			topdir + "/src/mochi_runtime/__init__.py",
		} {
			if !sliceContains(entries, want) {
				t.Errorf("expected %s in sdist; entries: %v", want, entries)
			}
		}
	})

	t.Run("wheel_is_deterministic", func(t *testing.T) {
		out1 := t.TempDir()
		out2 := t.TempDir()
		d1 := &Driver{CacheDir: t.TempDir()}
		d2 := &Driver{CacheDir: t.TempDir()}
		if err := d1.Build(fixture, out1, TargetPythonWheel); err != nil {
			t.Fatalf("Build wheel 1: %v", err)
		}
		if err := d2.Build(fixture, out2, TargetPythonWheel); err != nil {
			t.Fatalf("Build wheel 2: %v", err)
		}
		pkgName := d1.packageName(fixture)
		w1, err := os.ReadFile(filepath.Join(out1, pkgName+"-"+distVersion+"-py3-none-any.whl"))
		if err != nil {
			t.Fatalf("read wheel 1: %v", err)
		}
		w2, err := os.ReadFile(filepath.Join(out2, pkgName+"-"+distVersion+"-py3-none-any.whl"))
		if err != nil {
			t.Fatalf("read wheel 2: %v", err)
		}
		if !bytes.Equal(w1, w2) {
			t.Errorf("two wheel builds produced different bytes (%d vs %d)", len(w1), len(w2))
		}
	})
}

func unzipTo(zipPath, dst string) error {
	r, err := zip.OpenReader(zipPath)
	if err != nil {
		return err
	}
	defer r.Close()
	for _, f := range r.File {
		target := filepath.Join(dst, f.Name)
		if err := os.MkdirAll(filepath.Dir(target), 0o755); err != nil {
			return err
		}
		rc, err := f.Open()
		if err != nil {
			return err
		}
		out, err := os.Create(target)
		if err != nil {
			rc.Close()
			return err
		}
		if _, err := io.Copy(out, rc); err != nil {
			out.Close()
			rc.Close()
			return err
		}
		out.Close()
		rc.Close()
	}
	return nil
}

func tarGzEntries(tgzPath string) ([]string, error) {
	f, err := os.Open(tgzPath)
	if err != nil {
		return nil, err
	}
	defer f.Close()
	gz, err := gzip.NewReader(f)
	if err != nil {
		return nil, err
	}
	defer gz.Close()
	tr := tar.NewReader(gz)
	var names []string
	for {
		h, err := tr.Next()
		if err == io.EOF {
			break
		}
		if err != nil {
			return nil, err
		}
		names = append(names, strings.TrimPrefix(h.Name, "./"))
	}
	sort.Strings(names)
	return names, nil
}

func sliceContains(haystack []string, needle string) bool {
	for _, s := range haystack {
		if s == needle {
			return true
		}
	}
	return false
}
