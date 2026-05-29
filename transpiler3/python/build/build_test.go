package build

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"

	"mochi/transpiler3/python/lower"
)

// runPythonFixture compiles mochiPath to a TargetPythonSource tree, runs the
// resulting `python -m <pkg>` invocation against the runtime, and diffs
// stdout byte-for-byte against wantFile.
//
// Layout produced by Driver.Build:
//
//	out/
//	  pyproject.toml
//	  src/<pkg>/__init__.py
//	  src/<pkg>/__main__.py
//	  src/<pkg>/generated/<module>.py
//
// PYTHONPATH = out/src + runtime/python so the generated module can
// import `mochi_runtime.io.Print` without a wheel install.
func runPythonFixture(t *testing.T, mochiPath, wantFile string) {
	t.Helper()

	want, err := os.ReadFile(wantFile)
	if err != nil {
		t.Fatalf("read want file %s: %v", wantFile, err)
	}

	outDir := t.TempDir()
	d := &Driver{CacheDir: t.TempDir()}
	if err := d.Build(mochiPath, outDir, TargetPythonSource); err != nil {
		t.Fatalf("Build(%s): %v", filepath.Base(mochiPath), err)
	}

	pkgName := d.packageName(mochiPath)
	pkgInit := filepath.Join(outDir, "src", pkgName, "__init__.py")
	if _, err := os.Stat(pkgInit); err != nil {
		t.Fatalf("expected %s to exist: %v", pkgInit, err)
	}
	generated := filepath.Join(outDir, "src", pkgName, "generated", lower.ModuleName(mochiPath)+".py")
	if _, err := os.Stat(generated); err != nil {
		t.Fatalf("expected %s to exist: %v", generated, err)
	}

	tc, err := resolveToolchain()
	if err != nil {
		t.Fatalf("resolveToolchain: %v", err)
	}

	rtDir, err := runtimeDir()
	if err != nil {
		t.Fatalf("runtimeDir: %v", err)
	}

	cmd := exec.Command(tc.Python, "-m", pkgName)
	env := append(os.Environ(),
		"PYTHONPATH="+filepath.Join(outDir, "src")+string(os.PathListSeparator)+rtDir,
		"PYTHONDONTWRITEBYTECODE=1",
	)
	// Phase 13.0: cassette-mode LLM playback. When a `cassette/`
	// directory sits next to the .mochi fixture, point the runtime at
	// it via MOCHI_LLM_CASSETTE_DIR so mochi_llm_generate resolves the
	// hashed filename. Fixtures with no cassette dir get nothing set,
	// which surfaces the Phase 13.0 stderr diagnostic on live attempts.
	cassette := filepath.Join(filepath.Dir(mochiPath), "cassette")
	if fi, err := os.Stat(cassette); err == nil && fi.IsDir() {
		env = append(env, "MOCHI_LLM_CASSETTE_DIR="+cassette)
	}
	cmd.Env = env
	var stdout bytes.Buffer
	var stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		t.Fatalf("python -m %s: %v\nstderr: %s", pkgName, err, stderr.String())
	}

	norm := func(b []byte) []byte { return bytes.ReplaceAll(b, []byte("\r\n"), []byte("\n")) }
	got := norm(stdout.Bytes())
	want = norm(want)
	if !bytes.Equal(got, want) {
		t.Errorf("stdout mismatch\ngot:  %q\nwant: %q\nstderr: %s",
			got, want, strings.TrimSpace(stderr.String()))
	}
}
