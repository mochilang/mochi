// Package build is the Rust transpiler pipeline driver.
// Entry point: Driver.Build(src, outDir string, target Target) (string, error).
package build

import (
	"crypto/sha256"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"

	"mochi/parser"
	clower "mochi/transpiler3/c/lower"
	"mochi/transpiler3/rust/colour"
	"mochi/transpiler3/rust/emit"
	"mochi/transpiler3/rust/lower"
	"mochi/types"
)

// Target selects the Rust build output format.
type Target int

const (
	// TargetNativeExecutable: cargo build --release on the host triple,
	// returns the path to the binary.
	TargetNativeExecutable Target = iota
	// TargetRustSource: emit .rs only, no cargo invocation.
	TargetRustSource
	// TargetLinuxStaticX64: cargo zigbuild --target x86_64-unknown-linux-musl.
	TargetLinuxStaticX64
	// TargetLinuxStaticArm64: cargo zigbuild --target aarch64-unknown-linux-musl.
	TargetLinuxStaticArm64
	// TargetWasm32WASI: cargo build --target wasm32-wasi.
	TargetWasm32WASI
	// TargetRustCrate: emit the full Cargo crate (Cargo.toml + src/ +
	// any sidecar cffi/build.rs) into outDir without invoking cargo.
	// Used by Phase 15 for publish-ready crate inspection.
	TargetRustCrate
)

// Driver is the Rust transpiler pipeline entry point.
type Driver struct {
	// CacheDir overrides the default ~/.cache/mochi/rust/ location.
	CacheDir string
	// NoCache disables the build cache.
	NoCache bool
	// Deterministic enables reproducible builds (SOURCE_DATE_EPOCH=0,
	// fixed content-addressed workDir, build-id stripping).
	Deterministic bool

	cargoPath string
}

// Build compiles src to the given target artefact.
func (d *Driver) Build(src, outDir string, target Target) (string, error) {
	if d.cargoPath == "" {
		p, err := resolveCargo()
		if err != nil {
			return "", err
		}
		d.cargoPath = p
	}

	srcBytes, err := os.ReadFile(src)
	if err != nil {
		return "", fmt.Errorf("rust build: read %s: %w", src, err)
	}

	crateName := lower.CrateName(src)

	if target == TargetNativeExecutable && !d.NoCache {
		cacheKey := d.cacheKey(srcBytes)
		cacheEntry := filepath.Join(d.effectiveCacheDir(), cacheKey)
		if _, err := os.Stat(cacheEntry); err == nil {
			if err := os.MkdirAll(outDir, 0o755); err != nil {
				return "", err
			}
			dest := filepath.Join(outDir, "mochi_out")
			if err := copyFile(dest, cacheEntry); err != nil {
				return "", err
			}
			if err := os.Chmod(dest, 0o755); err != nil {
				return "", err
			}
			return dest, nil
		}
	}

	ast, err := parser.Parse(src)
	if err != nil {
		return "", fmt.Errorf("rust build: parse: %w", err)
	}

	if errs := types.Check(ast, types.NewEnv(nil)); len(errs) > 0 {
		return "", fmt.Errorf("rust build: typecheck: %w", errs[0])
	}

	prog, err := clower.Lower(ast)
	if err != nil {
		return "", fmt.Errorf("rust build: aotir lower: %w", err)
	}

	colours := colour.Analyse(prog)
	file, err := lower.Lower(prog, colours, crateName)
	if err != nil {
		return "", fmt.Errorf("rust build: rust lower: %w", err)
	}

	var workDir string
	if d.Deterministic || os.Getenv("MOCHI_DETERMINISTIC") == "1" {
		h := sha256.Sum256(srcBytes)
		workDir = filepath.Join(os.TempDir(), fmt.Sprintf("mochi-rust-det-%x", h[:8]))
		if mkErr := os.MkdirAll(workDir, 0o755); mkErr != nil {
			return "", mkErr
		}
		_ = os.RemoveAll(filepath.Join(workDir, "target"))
	} else {
		var mkErr error
		workDir, mkErr = os.MkdirTemp("", "mochi-rust-*")
		if mkErr != nil {
			return "", mkErr
		}
		defer os.RemoveAll(workDir)
	}

	// Generated crate layout: workDir/Cargo.toml + workDir/src/main.rs.
	srcDir := filepath.Join(workDir, "src")
	mainRs, err := emit.Emit(file, srcDir, "main")
	if err != nil {
		return "", fmt.Errorf("rust build: emit: %w", err)
	}

	if target == TargetRustSource {
		if err := os.MkdirAll(outDir, 0o755); err != nil {
			return "", err
		}
		dest := filepath.Join(outDir, crateName+".rs")
		if err := copyFile(dest, mainRs); err != nil {
			return "", err
		}
		return dest, nil
	}

	runtimeDir := runtimeSourceDir()
	if runtimeDir == "" {
		return "", fmt.Errorf("rust build: cannot locate mochi-runtime sources")
	}

	// Phase 12 (FFI): discover sidecar *.c files in the source directory
	// and stage them as a compiled C archive via build.rs + cc-rs.
	srcStem := strings.TrimSuffix(filepath.Base(src), ".mochi")
	cFiles, err := copySidecarCFiles(filepath.Dir(src), srcStem, workDir)
	if err != nil {
		return "", fmt.Errorf("rust build: stage sidecar C: %w", err)
	}
	withFFI := len(cFiles) > 0
	if withFFI {
		if err := os.WriteFile(filepath.Join(workDir, "build.rs"), []byte(generateBuildRs(cFiles)), 0o644); err != nil {
			return "", fmt.Errorf("rust build: write build.rs: %w", err)
		}
	}

	cargoToml := generateCargoToml(crateName, runtimeDir, withFFI)
	if err := os.WriteFile(filepath.Join(workDir, "Cargo.toml"), []byte(cargoToml), 0o644); err != nil {
		return "", fmt.Errorf("rust build: write Cargo.toml: %w", err)
	}

	if target == TargetRustCrate {
		if err := os.MkdirAll(outDir, 0o755); err != nil {
			return "", err
		}
		if err := copyTree(workDir, outDir); err != nil {
			return "", fmt.Errorf("rust build: copy crate: %w", err)
		}
		return outDir, nil
	}

	buildArgs := []string{"build", "--release"}
	switch target {
	case TargetLinuxStaticX64:
		buildArgs = []string{"zigbuild", "--release", "--target", "x86_64-unknown-linux-musl"}
	case TargetLinuxStaticArm64:
		buildArgs = []string{"zigbuild", "--release", "--target", "aarch64-unknown-linux-musl"}
	case TargetWasm32WASI:
		buildArgs = append(buildArgs, "--target", "wasm32-wasi")
	}

	buildCmd := exec.Command(d.cargoPath, buildArgs...)
	buildCmd.Dir = workDir
	buildCmd.Stdout = os.Stderr
	buildCmd.Stderr = os.Stderr
	if d.Deterministic || os.Getenv("MOCHI_DETERMINISTIC") == "1" {
		buildCmd.Env = append(os.Environ(),
			"SOURCE_DATE_EPOCH=0",
			"CARGO_BUILD_RUSTFLAGS=-C strip=symbols",
		)
	}
	if err := buildCmd.Run(); err != nil {
		return "", fmt.Errorf("rust build: cargo build: %w", err)
	}

	binName := crateName
	if target == TargetWasm32WASI {
		binName = crateName + ".wasm"
	}
	binPath, err := findBinary(workDir, target, crateName, binName)
	if err != nil {
		return "", fmt.Errorf("rust build: find binary: %w", err)
	}

	if err := os.MkdirAll(outDir, 0o755); err != nil {
		return "", err
	}
	dest := filepath.Join(outDir, "mochi_out")
	if target == TargetWasm32WASI {
		dest = filepath.Join(outDir, "mochi_out.wasm")
	}
	if err := copyFile(dest, binPath); err != nil {
		return "", err
	}
	if err := os.Chmod(dest, 0o755); err != nil {
		return "", err
	}

	if !d.NoCache && target == TargetNativeExecutable {
		cacheKey := d.cacheKey(srcBytes)
		cacheEntry := filepath.Join(d.effectiveCacheDir(), cacheKey)
		if err := os.MkdirAll(filepath.Dir(cacheEntry), 0o755); err == nil {
			_ = copyFile(cacheEntry, binPath)
		}
	}

	return dest, nil
}

func resolveCargo() (string, error) {
	if p := os.Getenv("CARGO_PATH"); p != "" {
		return p, nil
	}
	for _, candidate := range []string{
		os.ExpandEnv("$HOME/.cargo/bin/cargo"),
		"/usr/local/bin/cargo",
		"/usr/bin/cargo",
	} {
		if _, err := os.Stat(candidate); err == nil {
			return candidate, nil
		}
	}
	p, err := exec.LookPath("cargo")
	if err != nil {
		return "", fmt.Errorf("cargo not found on PATH (set CARGO_PATH or install rustup): %w", err)
	}
	return p, nil
}

func (d *Driver) effectiveCacheDir() string {
	if d.CacheDir != "" {
		return d.CacheDir
	}
	if c := os.Getenv("MOCHI_CACHE_DIR"); c != "" {
		return filepath.Join(c, "rust")
	}
	home, err := os.UserHomeDir()
	if err != nil {
		return os.TempDir()
	}
	return filepath.Join(home, ".cache", "mochi", "rust")
}

func (d *Driver) cacheKey(srcBytes []byte) string {
	h := sha256.New()
	h.Write(srcBytes)
	h.Write([]byte(d.cargoPath))
	if d.Deterministic {
		h.Write([]byte{1})
	} else {
		h.Write([]byte{0})
	}
	return fmt.Sprintf("%x", h.Sum(nil))
}

// runtimeSourceDir returns the absolute path to runtime3/rust/mochi-runtime/.
func runtimeSourceDir() string {
	_, thisFile, _, ok := runtime.Caller(0)
	if !ok {
		return ""
	}
	// thisFile: <root>/transpiler3/rust/build/build.go
	// runtime:  <root>/runtime3/rust/mochi-runtime/
	dir := filepath.Dir(thisFile)          // .../build
	rustDir := filepath.Dir(dir)           // .../rust
	transpilerDir := filepath.Dir(rustDir) // .../transpiler3
	root := filepath.Dir(transpilerDir)    // .../<repo>
	p := filepath.Join(root, "runtime3", "rust", "mochi-runtime")
	if _, err := os.Stat(p); err != nil {
		return ""
	}
	return p
}

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

func generateCargoToml(crateName, runtimePath string, withFFI bool) string {
	buildDeps := ""
	if withFFI {
		buildDeps = "\n[build-dependencies]\ncc = \"1\"\n"
	}
	return fmt.Sprintf(`[package]
name = %q
version = "0.1.0"
edition = "2024"
license = "Apache-2.0"
description = "Mochi-generated Rust crate."
repository = "https://github.com/mochilang/mochi"

[[bin]]
name = %q
path = "src/main.rs"

[dependencies]
mochi-runtime = { path = %q }
%s
[profile.release]
opt-level = 3
lto = "thin"
codegen-units = 1
panic = "unwind"
strip = "symbols"
`, crateName, crateName, runtimePath, buildDeps)
}

// copySidecarCFiles copies *.c files matching srcStem from srcDir into
// workDir/cffi/. Two patterns are accepted: `<stem>.c` (single file) and
// `<stem>_*.c` (multiple helpers per fixture). Returns the relative paths
// (under workDir) of the copied files so a build.rs script can compile
// them via cc-rs. Phase 12.
func copySidecarCFiles(srcDir, srcStem, workDir string) ([]string, error) {
	entries, err := os.ReadDir(srcDir)
	if err != nil {
		return nil, err
	}
	var copied []string
	for _, e := range entries {
		if e.IsDir() || !strings.HasSuffix(e.Name(), ".c") {
			continue
		}
		base := strings.TrimSuffix(e.Name(), ".c")
		if base != srcStem && !strings.HasPrefix(base, srcStem+"_") {
			continue
		}
		if len(copied) == 0 {
			if mkErr := os.MkdirAll(filepath.Join(workDir, "cffi"), 0o755); mkErr != nil {
				return nil, mkErr
			}
		}
		dst := filepath.Join(workDir, "cffi", e.Name())
		if err := copyFile(dst, filepath.Join(srcDir, e.Name())); err != nil {
			return nil, err
		}
		copied = append(copied, "cffi/"+e.Name())
	}
	return copied, nil
}

// generateBuildRs returns a build.rs that compiles the listed C files into
// one static archive via cc-rs. Each path is workDir-relative. Phase 12.
func generateBuildRs(cFiles []string) string {
	var sb strings.Builder
	sb.WriteString("fn main() {\n")
	sb.WriteString("    let mut build = cc::Build::new();\n")
	for _, f := range cFiles {
		fmt.Fprintf(&sb, "    build.file(%q);\n", f)
		fmt.Fprintf(&sb, "    println!(\"cargo:rerun-if-changed={}\", %q);\n", f)
	}
	sb.WriteString("    build.compile(\"mochi_ffi\");\n")
	sb.WriteString("}\n")
	return sb.String()
}

func findBinary(workDir string, target Target, crateName, binName string) (string, error) {
	var subPath string
	switch target {
	case TargetLinuxStaticX64:
		subPath = filepath.Join("target", "x86_64-unknown-linux-musl", "release", binName)
	case TargetLinuxStaticArm64:
		subPath = filepath.Join("target", "aarch64-unknown-linux-musl", "release", binName)
	case TargetWasm32WASI:
		subPath = filepath.Join("target", "wasm32-wasi", "release", binName)
	default:
		subPath = filepath.Join("target", "release", binName)
	}
	p := filepath.Join(workDir, subPath)
	if _, err := os.Stat(p); err == nil {
		return p, nil
	}
	// Fallback walk.
	root := filepath.Join(workDir, "target")
	var found string
	_ = filepath.Walk(root, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return nil
		}
		if info.IsDir() {
			return nil
		}
		base := filepath.Base(path)
		if base == binName && strings.Contains(path, "release") {
			found = path
		}
		return nil
	})
	if found != "" {
		return found, nil
	}
	return "", fmt.Errorf("binary %s not found under %s", binName, root)
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

// copyTree recursively copies srcDir into dstDir, skipping the cargo
// target directory. Used by TargetRustCrate (Phase 15) to export a
// publish-ready crate layout to the caller's outDir.
func copyTree(srcDir, dstDir string) error {
	return filepath.Walk(srcDir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		rel, err := filepath.Rel(srcDir, path)
		if err != nil {
			return err
		}
		if rel == "." {
			return nil
		}
		if info.IsDir() {
			if rel == "target" {
				return filepath.SkipDir
			}
			return os.MkdirAll(filepath.Join(dstDir, rel), 0o755)
		}
		return copyFile(filepath.Join(dstDir, rel), path)
	})
}
