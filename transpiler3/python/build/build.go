package build

import (
	"crypto/sha256"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strconv"
	"strings"

	"mochi/parser"
	clower "mochi/transpiler3/c/lower"
	"mochi/transpiler3/python/emit"
	"mochi/transpiler3/python/lower"
	"mochi/types"
)

// Target selects the Python packaging format.
type Target int

const (
	// TargetPythonSource emits a `src/<pkg>/` source tree only (Phase 1).
	TargetPythonSource Target = iota
	// TargetPythonWheel emits a built wheel (.whl) via `uv build`. Phase 15.
	TargetPythonWheel
	// TargetPythonSdist emits a source distribution (.tar.gz). Phase 15.
	TargetPythonSdist
	// TargetPythonApp emits a zipapp or PEX. Phase 17.
	TargetPythonApp
	// TargetPythonIpykernel emits a Jupyter ipykernel package. Phase 17.
	TargetPythonIpykernel
)

// Toolchain holds the resolved python binary and version.
type Toolchain struct {
	Python string // absolute path to python3 binary
	Major  int
	Minor  int
	Patch  int
}

// resolveToolchain finds python3 on PATH and parses --version.
// Phase 1 floor is CPython 3.12.0 per MEP-51 §Targets.
func resolveToolchain() (*Toolchain, error) {
	var pyPath string
	if p := os.Getenv("MOCHI_PYTHON"); p != "" {
		if _, err := os.Stat(p); err == nil {
			pyPath = p
		}
	}
	if pyPath == "" {
		var err error
		pyPath, err = exec.LookPath("python3")
		if err != nil {
			pyPath, err = exec.LookPath("python")
			if err != nil {
				return nil, fmt.Errorf("python3 not found on PATH: %w", err)
			}
		}
	}

	out, err := exec.Command(pyPath, "--version").Output()
	if err != nil {
		return nil, fmt.Errorf("python --version: %w", err)
	}
	ver := strings.TrimSpace(string(out))
	ver = strings.TrimPrefix(ver, "Python ")
	parts := strings.SplitN(ver, ".", 3)
	if len(parts) < 3 {
		return nil, fmt.Errorf("unexpected python --version output: %q", string(out))
	}
	major, err := strconv.Atoi(parts[0])
	if err != nil {
		return nil, fmt.Errorf("python major: %w", err)
	}
	if major < 3 {
		return nil, fmt.Errorf("python 3.12+ required; found %s", ver)
	}
	minor, err := strconv.Atoi(parts[1])
	if err != nil {
		return nil, fmt.Errorf("python minor: %w", err)
	}
	if major == 3 && minor < 12 {
		return nil, fmt.Errorf("python 3.12+ required; found %s", ver)
	}
	patchStr := parts[2]
	if i := strings.IndexFunc(patchStr, func(r rune) bool {
		return r < '0' || r > '9'
	}); i >= 0 {
		patchStr = patchStr[:i]
	}
	patch, err := strconv.Atoi(patchStr)
	if err != nil {
		return nil, fmt.Errorf("python patch: %w", err)
	}

	return &Toolchain{Python: pyPath, Major: major, Minor: minor, Patch: patch}, nil
}

// Driver is the Python transpiler pipeline entry point.
type Driver struct {
	// CacheDir overrides the default ~/.cache/mochi/python/ location.
	CacheDir string
	// NoCache disables the build cache.
	NoCache bool
	// PackagePrefix overrides the default `mochi_user_` package prefix.
	PackagePrefix string
	tc            *Toolchain
}

// Build compiles src to a Python project tree at out for the given target.
func (d *Driver) Build(src, out string, target Target) error {
	if d.tc == nil {
		tc, err := resolveToolchain()
		if err != nil {
			return err
		}
		d.tc = tc
	}

	srcBytes, err := os.ReadFile(src)
	if err != nil {
		return fmt.Errorf("python build: read %s: %w", src, err)
	}

	cacheKey := d.cacheKey(srcBytes)
	if !d.NoCache && target == TargetPythonSource {
		cacheEntry := filepath.Join(d.effectiveCacheDir(), cacheKey)
		if fi, err := os.Stat(cacheEntry); err == nil && fi.IsDir() {
			return copyTree(out, cacheEntry)
		}
	}

	ast, err := parser.Parse(src)
	if err != nil {
		return fmt.Errorf("python build: parse: %w", err)
	}
	if errs := types.Check(ast, types.NewEnv(nil)); len(errs) > 0 {
		return fmt.Errorf("python build: typecheck: %w", errs[0])
	}

	prog, err := clower.Lower(ast)
	if err != nil {
		return fmt.Errorf("python build: aotir lower: %w", err)
	}

	moduleName := lower.ModuleName(src)
	pkgName := d.packageName(src)

	pyMod, err := lower.Lower(prog, moduleName)
	if err != nil {
		return fmt.Errorf("python build: python lower: %w", err)
	}

	workDir, err := os.MkdirTemp("", "mochi-python-*")
	if err != nil {
		return err
	}
	defer os.RemoveAll(workDir)

	pkgDir := filepath.Join(workDir, "src", pkgName)
	generatedDir := filepath.Join(pkgDir, "generated")
	if err := os.MkdirAll(generatedDir, 0o755); err != nil {
		return err
	}

	if err := emit.Emit(pyMod, filepath.Join(generatedDir, moduleName+".py")); err != nil {
		return err
	}
	if err := writePackageLayout(workDir, pkgName, moduleName); err != nil {
		return err
	}

	// Phase 12.0: when the program declares `extern python fun` entries,
	// copy the sidecar `<moduleName>_externs.py` from next to the .mochi
	// source into `src/<pkgName>_externs.py` so the generated module's
	// `from <pkgName>_externs import ...` resolves. Missing-sidecar is a
	// build error: the FFI surface is opt-in and an undeclared sidecar
	// is the user's mistake to fix.
	if len(prog.PythonFuncs) > 0 {
		sidecar := filepath.Join(filepath.Dir(src), moduleName+"_externs.py")
		if _, err := os.Stat(sidecar); err != nil {
			return fmt.Errorf("python build: extern python fun declared but sidecar %s not found: %w", sidecar, err)
		}
		dst := filepath.Join(workDir, "src", pkgName+"_externs.py")
		if err := copyFile(dst, sidecar); err != nil {
			return fmt.Errorf("python build: copy externs sidecar: %w", err)
		}
	}

	if target != TargetPythonSource {
		return fmt.Errorf("python build: target %d not supported until later phase", target)
	}

	if err := copyTree(out, workDir); err != nil {
		return err
	}
	if !d.NoCache {
		cacheEntry := filepath.Join(d.effectiveCacheDir(), cacheKey)
		if err := os.MkdirAll(filepath.Dir(cacheEntry), 0o755); err == nil {
			_ = copyTree(cacheEntry, workDir)
		}
	}
	return nil
}

func (d *Driver) packageName(src string) string {
	prefix := d.PackagePrefix
	if prefix == "" {
		prefix = "mochi_user_"
	}
	return prefix + lower.ModuleName(src)
}

func (d *Driver) effectiveCacheDir() string {
	if d.CacheDir != "" {
		return d.CacheDir
	}
	if c := os.Getenv("MOCHI_CACHE_DIR"); c != "" {
		return filepath.Join(c, "python")
	}
	home, err := os.UserHomeDir()
	if err != nil {
		return os.TempDir()
	}
	return filepath.Join(home, ".cache", "mochi", "python")
}

func (d *Driver) cacheKey(srcBytes []byte) string {
	h := sha256.New()
	h.Write(srcBytes)
	if d.tc != nil {
		fmt.Fprintf(h, "%d.%d.%d", d.tc.Major, d.tc.Minor, d.tc.Patch)
	}
	h.Write([]byte("mep51-phase12"))
	return fmt.Sprintf("%x", h.Sum(nil))
}

// runtimeDir locates the on-disk runtime/python/ root by walking up
// from this file. Tests use this to prepend the runtime onto PYTHONPATH.
func runtimeDir() (string, error) {
	_, thisFile, _, ok := runtime.Caller(0)
	if !ok {
		return "", fmt.Errorf("runtime.Caller(0) failed")
	}
	dir := filepath.Dir(thisFile)
	for {
		candidate := filepath.Join(dir, "runtime", "python")
		if fi, err := os.Stat(candidate); err == nil && fi.IsDir() {
			return candidate, nil
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			return "", fmt.Errorf("runtime/python not found walking up from %s", thisFile)
		}
		dir = parent
	}
}

// repoRootForBuild walks up to the repo root (where go.mod lives).
func repoRootForBuild(t interface {
	Helper()
	Fatalf(string, ...any)
}) string {
	t.Helper()
	_, thisFile, _, ok := runtime.Caller(0)
	if !ok {
		t.Fatalf("runtime.Caller(0) failed")
	}
	dir := filepath.Dir(thisFile)
	for {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			t.Fatalf("go.mod not found walking up from %s", thisFile)
		}
		dir = parent
	}
}

func copyFile(dst, src string) error {
	if err := os.MkdirAll(filepath.Dir(dst), 0o755); err != nil {
		return err
	}
	in, err := os.Open(src)
	if err != nil {
		return err
	}
	defer in.Close()
	out, err := os.Create(dst)
	if err != nil {
		return err
	}
	defer out.Close()
	_, err = io.Copy(out, in)
	return err
}

// copyTree recursively copies srcDir's contents into dstDir.
func copyTree(dstDir, srcDir string) error {
	return filepath.Walk(srcDir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		rel, err := filepath.Rel(srcDir, path)
		if err != nil {
			return err
		}
		target := filepath.Join(dstDir, rel)
		if info.IsDir() {
			return os.MkdirAll(target, info.Mode())
		}
		return copyFile(target, path)
	})
}
