package build

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"testing"
)

// TestCLIHello exercises the Phase 1.1 CLI surface end-to-end:
// `go run mochi/cmd/mochi build --target=c-aot --out=<bin>
// [--emit=c] tests/transpiler3/c/fixtures/hello/hello.mochi`,
// then runs the produced binary and diffs stdout against
// expect.txt. The --emit=c sub-case additionally asserts that
// the generated C source landed next to the binary.
func TestCLIHello(t *testing.T) {
	if runtime.GOOS == "windows" {
		t.Skip("Phase 1.1 ships POSIX host-cc discovery only; Windows lands in Phase 11")
	}
	root := repoRoot(t)
	fixture := filepath.Join(root, "tests", "transpiler3", "c", "fixtures", "hello")
	src := filepath.Join(fixture, "hello.mochi")
	want, err := os.ReadFile(filepath.Join(fixture, "expect.txt"))
	if err != nil {
		t.Fatalf("read expect.txt: %v", err)
	}

	mochiBin := filepath.Join(t.TempDir(), "mochi")
	build := exec.Command("go", "build", "-o", mochiBin, "mochi/cmd/mochi")
	build.Dir = root
	if out, err := build.CombinedOutput(); err != nil {
		t.Fatalf("go build cmd/mochi: %v\n%s", err, out)
	}

	cases := []struct {
		name     string
		emit     string
		wantSrc  bool
	}{
		{name: "executable", emit: "", wantSrc: false},
		{name: "emit-c", emit: "c", wantSrc: true},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			outBin := filepath.Join(t.TempDir(), "hello")
			args := []string{"build", "--target=c-aot", "--out=" + outBin}
			if tc.emit != "" {
				args = append(args, "--emit="+tc.emit)
			}
			args = append(args, src)

			cmd := exec.Command(mochiBin, args...)
			cmd.Dir = root
			cmd.Env = append(os.Environ(), "MOCHI_CACHE_DIR="+t.TempDir())
			var stdout, stderr bytes.Buffer
			cmd.Stdout = &stdout
			cmd.Stderr = &stderr
			if err := cmd.Run(); err != nil {
				t.Fatalf("mochi %s: %v\nstdout:\n%s\nstderr:\n%s",
					strings.Join(args, " "), err, stdout.String(), stderr.String())
			}
			if !strings.Contains(stdout.String(), "binary "+outBin) {
				t.Fatalf("missing 'binary <path>' in stdout:\n%s", stdout.String())
			}
			if tc.wantSrc {
				wantSrcLine := "source " + outBin + ".c"
				if !strings.Contains(stdout.String(), wantSrcLine) {
					t.Fatalf("missing %q in stdout:\n%s", wantSrcLine, stdout.String())
				}
				if _, err := os.Stat(outBin + ".c"); err != nil {
					t.Fatalf("stat emitted .c: %v", err)
				}
			} else {
				if _, err := os.Stat(outBin + ".c"); err == nil {
					t.Fatalf("--emit=executable left a stray %s.c", outBin)
				}
			}

			run := exec.Command(outBin)
			var rstdout bytes.Buffer
			run.Stdout = &rstdout
			run.Stderr = os.Stderr
			if err := run.Run(); err != nil {
				t.Fatalf("run %s: %v", outBin, err)
			}
			if got := rstdout.String(); got != string(want) {
				t.Fatalf("stdout mismatch:\n--- want ---\n%q\n--- got ---\n%q", string(want), got)
			}
		})
	}

	t.Run("cache-hit", func(t *testing.T) {
		cacheDir := t.TempDir()
		outBin := filepath.Join(t.TempDir(), "hello")
		args := []string{"build", "--target=c-aot", "--out=" + outBin, src}

		first := exec.Command(mochiBin, args...)
		first.Dir = root
		first.Env = append(os.Environ(), "MOCHI_CACHE_DIR="+cacheDir)
		var fstdout, fstderr bytes.Buffer
		first.Stdout = &fstdout
		first.Stderr = &fstderr
		if err := first.Run(); err != nil {
			t.Fatalf("first build: %v\nstdout:\n%s\nstderr:\n%s",
				err, fstdout.String(), fstderr.String())
		}
		if !strings.Contains(fstdout.String(), "binary "+outBin) {
			t.Fatalf("first build expected 'binary <path>', got:\n%s", fstdout.String())
		}

		outBin2 := filepath.Join(t.TempDir(), "hello2")
		args2 := []string{"build", "--target=c-aot", "--out=" + outBin2, src}
		second := exec.Command(mochiBin, args2...)
		second.Dir = root
		second.Env = append(os.Environ(), "MOCHI_CACHE_DIR="+cacheDir)
		var sstdout, sstderr bytes.Buffer
		second.Stdout = &sstdout
		second.Stderr = &sstderr
		if err := second.Run(); err != nil {
			t.Fatalf("second build: %v\nstdout:\n%s\nstderr:\n%s",
				err, sstdout.String(), sstderr.String())
		}
		if !strings.Contains(sstdout.String(), "cached "+outBin2) {
			t.Fatalf("second build expected 'cached <path>', got:\n%s", sstdout.String())
		}
	})
}
