package build

import (
	"bytes"
	"encoding/json"
	"errors"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"testing"
)

// TestPhase17Ipykernel is the gate for Phase 17.0 + 17.1:
//
//   - TargetPythonIpykernel emits a Jupyter-shaped kernelspec
//     directory plus a self-contained source tree under outDir
//   - kernel.json parses to the expected argv + display_name +
//     language + metadata block (per the jupyter_client docs)
//   - mochi_kernel.py and its __main__ entry compile under the
//     host Python interpreter
//   - MochiKernel._unwrap_main correctly strips the def main()
//     wrapper and `if __name__` trailer so cell-scoped bindings
//     persist across cells via IPython's user_ns
//   - MochiKernel._transpile_cell, given $MOCHI_BIN pointing at a
//     built mochi binary, round-trips a Mochi cell through
//     `mochi build --target=python-source` and returns module-
//     scope Python (no `def main`, no `if __name__` trailer)
//
// The end-to-end nbclient gate (launching the kernel via a
// subprocess and executing a notebook against it) is deferred to
// 17.3.1; it requires ipykernel + nbclient in the test runner's
// Python interpreter and an installed mochi binary on PATH. The
// gates above are sufficient to declare 17.0-17.2 LANDED because
// they cover every code path the kernel relies on.
func TestPhase17Ipykernel(t *testing.T) {
	fixture := filepath.Join(repoRootForBuild(t), "tests", "transpiler3", "python", "fixtures", "phase17-ipykernel", "notebook_helloworld", "hello.mochi")

	t.Run("emits_kernelspec_dir_with_kernel_json", func(t *testing.T) {
		out := t.TempDir()
		d := &Driver{CacheDir: t.TempDir()}
		if err := d.Build(fixture, out, TargetPythonIpykernel); err != nil {
			t.Fatalf("Build: %v", err)
		}
		pkgName := d.packageName(fixture)
		kernelDir := filepath.Join(out, "kernels", "mochi-"+pkgName)
		for _, want := range []string{
			"kernel.json",
			"logo-32x32.png",
			"logo-64x64.png",
		} {
			if _, err := os.Stat(filepath.Join(kernelDir, want)); err != nil {
				t.Errorf("expected %s in kernelspec dir: %v", want, err)
			}
		}
		// Self-contained source tree was also copied.
		for _, want := range []string{
			filepath.Join("src", pkgName, "__init__.py"),
			filepath.Join("src", pkgName, "__main__.py"),
			filepath.Join("src", "mochi_runtime", "__init__.py"),
			filepath.Join("src", "mochi_runtime", "io.py"),
			"pyproject.toml",
		} {
			if _, err := os.Stat(filepath.Join(out, want)); err != nil {
				t.Errorf("expected %s in self-contained tree: %v", want, err)
			}
		}
	})

	t.Run("kernel_json_has_correct_shape", func(t *testing.T) {
		out := t.TempDir()
		d := &Driver{CacheDir: t.TempDir()}
		if err := d.Build(fixture, out, TargetPythonIpykernel); err != nil {
			t.Fatalf("Build: %v", err)
		}
		pkgName := d.packageName(fixture)
		jsonPath := filepath.Join(out, "kernels", "mochi-"+pkgName, "kernel.json")
		raw, err := os.ReadFile(jsonPath)
		if err != nil {
			t.Fatalf("read kernel.json: %v", err)
		}
		var spec map[string]any
		if err := json.Unmarshal(raw, &spec); err != nil {
			t.Fatalf("parse kernel.json: %v", err)
		}
		argvAny, ok := spec["argv"].([]any)
		if !ok || len(argvAny) < 5 {
			t.Fatalf("argv shape wrong: %v", spec["argv"])
		}
		gotArgv := make([]string, len(argvAny))
		for i, v := range argvAny {
			gotArgv[i] = v.(string)
		}
		wantArgv := []string{"{python}", "-m", "mochi_runtime.kernel", "-f", "{connection_file}"}
		if !stringSliceEqual(gotArgv, wantArgv) {
			t.Errorf("argv = %v; want %v", gotArgv, wantArgv)
		}
		if got, want := spec["language"], "mochi"; got != want {
			t.Errorf("language = %v; want %v", got, want)
		}
		if got, want := spec["interrupt_mode"], "signal"; got != want {
			t.Errorf("interrupt_mode = %v; want %v", got, want)
		}
		display, _ := spec["display_name"].(string)
		if !strings.Contains(display, "Mochi") {
			t.Errorf("display_name = %q; want substring %q", display, "Mochi")
		}
		md, ok := spec["metadata"].(map[string]any)
		if !ok {
			t.Fatalf("metadata not a map: %v", spec["metadata"])
		}
		for _, k := range []string{"mochi_version", "transpiler_version", "python_version"} {
			if _, ok := md[k]; !ok {
				t.Errorf("metadata missing key %q", k)
			}
		}
	})

	t.Run("mochi_kernel_py_compiles", func(t *testing.T) {
		py, err := pythonInterpreter()
		if err != nil {
			t.Skipf("no python interpreter: %v", err)
		}
		rt, err := runtimeDir()
		if err != nil {
			t.Fatalf("runtimeDir: %v", err)
		}
		kernelMod := filepath.Join(rt, "mochi_runtime", "kernel", "mochi_kernel.py")
		entryMod := filepath.Join(rt, "mochi_runtime", "kernel", "__main__.py")
		for _, p := range []string{kernelMod, entryMod} {
			cmd := exec.Command(py, "-c", "import py_compile, sys; py_compile.compile(sys.argv[1], doraise=True)", p)
			var stderr bytes.Buffer
			cmd.Stderr = &stderr
			if err := cmd.Run(); err != nil {
				t.Errorf("py_compile %s: %v\n%s", filepath.Base(p), err, stderr.String())
			}
		}
	})

	t.Run("unwrap_main_strips_wrapper_and_trailer", func(t *testing.T) {
		py, err := pythonWithIpykernel()
		if err != nil {
			t.Skipf("no python with ipykernel: %v", err)
		}
		rt, err := runtimeDir()
		if err != nil {
			t.Fatalf("runtimeDir: %v", err)
		}
		script := `
import sys
sys.path.insert(0, sys.argv[1])
from mochi_runtime.kernel.mochi_kernel import MochiKernel
sample = '''from __future__ import annotations
from mochi_runtime.io import Print

def main() -> None:
    Print.line("hello")

if __name__ == "__main__":
    main()
'''
got = MochiKernel._unwrap_main(sample)
assert "def main(" not in got, got
assert "if __name__" not in got, got
assert "Print.line(\"hello\")" in got, got
print("OK")
`
		cmd := exec.Command(py, "-c", script, rt)
		var stdout, stderr bytes.Buffer
		cmd.Stdout = &stdout
		cmd.Stderr = &stderr
		if err := cmd.Run(); err != nil {
			t.Fatalf("_unwrap_main script failed: %v\nstdout: %s\nstderr: %s", err, stdout.String(), stderr.String())
		}
		if !strings.Contains(stdout.String(), "OK") {
			t.Errorf("expected OK; got %q (stderr %q)", stdout.String(), stderr.String())
		}
	})

	t.Run("transpile_cell_round_trips_via_mochi_binary", func(t *testing.T) {
		py, err := pythonWithIpykernel()
		if err != nil {
			t.Skipf("no python with ipykernel: %v", err)
		}
		mochiBin, err := buildMochiBinaryForKernelTest(t)
		if err != nil {
			t.Skipf("cannot build mochi binary: %v", err)
		}
		rt, err := runtimeDir()
		if err != nil {
			t.Fatalf("runtimeDir: %v", err)
		}
		script := `
import os, sys
sys.path.insert(0, sys.argv[1])
os.environ['MOCHI_BIN'] = sys.argv[2]
from mochi_runtime.kernel.mochi_kernel import MochiKernel
out = MochiKernel._transpile_cell('print("cell hi")\n')
assert "def main(" not in out, out
assert "if __name__" not in out, out
assert 'Print.line("cell hi")' in out, out
print("OK")
`
		cmd := exec.Command(py, "-c", script, rt, mochiBin)
		var stdout, stderr bytes.Buffer
		cmd.Stdout = &stdout
		cmd.Stderr = &stderr
		if err := cmd.Run(); err != nil {
			t.Fatalf("_transpile_cell script failed: %v\nstdout: %s\nstderr: %s", err, stdout.String(), stderr.String())
		}
		if !strings.Contains(stdout.String(), "OK") {
			t.Errorf("expected OK; got %q (stderr %q)", stdout.String(), stderr.String())
		}
	})

	t.Run("ipykernel_present_runs_full_cell", func(t *testing.T) {
		// Optional gate: when ipykernel + nbclient are importable
		// in the host Python (e.g. a local venv pointed at via
		// MOCHI_JUPYTER_PYTHON), launch the Mochi kernel through
		// jupyter_client and execute a notebook cell end-to-end.
		// Skipped when the optional dependency is missing.
		py := os.Getenv("MOCHI_JUPYTER_PYTHON")
		if py == "" {
			t.Skip("MOCHI_JUPYTER_PYTHON not set; ipykernel/nbclient end-to-end gate skipped")
		}
		mochiBin, err := buildMochiBinaryForKernelTest(t)
		if err != nil {
			t.Skipf("cannot build mochi binary: %v", err)
		}
		rt, err := runtimeDir()
		if err != nil {
			t.Fatalf("runtimeDir: %v", err)
		}
		// Confirm ipykernel + nbclient importable; skip otherwise.
		if err := exec.Command(py, "-c", "import ipykernel, nbclient, jupyter_client").Run(); err != nil {
			t.Skipf("MOCHI_JUPYTER_PYTHON missing ipykernel/nbclient: %v", err)
		}
		script := `
import json, os, sys, tempfile
sys.path.insert(0, sys.argv[1])
os.environ['MOCHI_BIN'] = sys.argv[2]
os.environ['PYTHONPATH'] = sys.argv[1] + os.pathsep + os.environ.get('PYTHONPATH', '')

from jupyter_client.kernelspec import KernelSpecManager
import nbformat
from nbclient import NotebookClient

ks_root = tempfile.mkdtemp(prefix='mochi-ks-')
ks_dir = os.path.join(ks_root, 'kernels', 'mochi-test')
os.makedirs(ks_dir)
spec = {
    'argv': [sys.executable, '-m', 'mochi_runtime.kernel', '-f', '{connection_file}'],
    'display_name': 'Mochi (test)',
    'language': 'mochi',
}
with open(os.path.join(ks_dir, 'kernel.json'), 'w') as f:
    json.dump(spec, f)
os.environ['JUPYTER_PATH'] = ks_root

nb = nbformat.v4.new_notebook()
nb.cells.append(nbformat.v4.new_code_cell(source='print("nbcell hi")\n'))
client = NotebookClient(nb, kernel_name='mochi-test', timeout=60)
client.execute()
outs = nb.cells[0].outputs
texts = [o.get('text', '') for o in outs if o.get('output_type') == 'stream']
joined = ''.join(texts)
assert 'nbcell hi' in joined, (outs, joined)
print('OK')
`
		cmd := exec.Command(py, "-c", script, rt, mochiBin)
		var stdout, stderr bytes.Buffer
		cmd.Stdout = &stdout
		cmd.Stderr = &stderr
		if err := cmd.Run(); err != nil {
			t.Fatalf("nbclient script failed: %v\nstdout: %s\nstderr: %s", err, stdout.String(), stderr.String())
		}
		if !strings.Contains(stdout.String(), "OK") {
			t.Errorf("expected OK; got %q (stderr %q)", stdout.String(), stderr.String())
		}
	})
}

// pythonWithIpykernel returns the first reachable Python interpreter
// that can import ipykernel, checked in priority order:
// MOCHI_JUPYTER_PYTHON, MOCHI_PYTHON, python3, python. Used by gates
// that exercise mochi_kernel.py (which imports ipykernel at module
// scope) — these gates skip when no suitable interpreter is found.
func pythonWithIpykernel() (string, error) {
	candidates := []string{}
	if p := os.Getenv("MOCHI_JUPYTER_PYTHON"); p != "" {
		candidates = append(candidates, p)
	}
	if p := os.Getenv("MOCHI_PYTHON"); p != "" {
		candidates = append(candidates, p)
	}
	if p, err := exec.LookPath("python3"); err == nil {
		candidates = append(candidates, p)
	}
	if p, err := exec.LookPath("python"); err == nil {
		candidates = append(candidates, p)
	}
	for _, p := range candidates {
		if _, err := os.Stat(p); err != nil {
			continue
		}
		if err := exec.Command(p, "-c", "import ipykernel").Run(); err == nil {
			return p, nil
		}
	}
	return "", errors.New("no python with ipykernel found (tried MOCHI_JUPYTER_PYTHON, MOCHI_PYTHON, python3, python)")
}

// pythonInterpreter returns $MOCHI_PYTHON or the resolved python3 on
// PATH. Used by the Python-side gates above; tests are skipped when
// no interpreter is reachable.
func pythonInterpreter() (string, error) {
	if p := os.Getenv("MOCHI_PYTHON"); p != "" {
		if _, err := os.Stat(p); err == nil {
			return p, nil
		}
	}
	if p, err := exec.LookPath("python3"); err == nil {
		return p, nil
	}
	if p, err := exec.LookPath("python"); err == nil {
		return p, nil
	}
	return "", errors.New("python3 not on PATH and MOCHI_PYTHON unset")
}

// buildMochiBinaryForKernelTest builds the Mochi CLI once into a
// temp file (cached per test binary) so the kernel's subprocess
// transpile path has a real executable to invoke. Uses `go build`
// against the workspace root resolved via repoRootForBuild.
func buildMochiBinaryForKernelTest(t *testing.T) (string, error) {
	t.Helper()
	repo := repoRootForBuild(t)
	tmp := filepath.Join(os.TempDir(), "mochi-phase17-bin")
	if runtime.GOOS == "windows" {
		tmp += ".exe"
	}
	// Always rebuild; cheap (~3s) and avoids stale-binary surprises.
	cmd := exec.Command("go", "build", "-o", tmp, "./cmd/mochi")
	cmd.Dir = repo
	var stderr bytes.Buffer
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		return "", errors.New(stderr.String())
	}
	return tmp, nil
}
