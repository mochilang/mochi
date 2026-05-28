// Package build is the Swift transpiler pipeline driver.
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
	"mochi/transpiler3/swift/colour"
	"mochi/transpiler3/swift/emit"
	"mochi/transpiler3/swift/lower"
	"mochi/types"
)

// Target selects the Swift build output format.
type Target int

const (
	TargetMacOSExecutable  Target = iota // swift build -c release → binary
	TargetSwiftSource                    // emit .swift only, no build
	TargetLinuxStaticX64                 // swift build -c release --swift-sdk x86_64-swift-linux-musl
	TargetLinuxStaticArm64               // swift build -c release --swift-sdk aarch64-swift-linux-musl
)

// Driver is the Swift transpiler pipeline entry point.
type Driver struct {
	// CacheDir overrides the default ~/.cache/mochi/swift/ location.
	CacheDir string
	// NoCache disables the build cache.
	NoCache bool
	// Deterministic enables reproducible builds by setting SWIFTPM_DETERMINISTIC_BUILD=1
	// and SOURCE_DATE_EPOCH=0 when invoking swift build. It also includes the flag
	// in the cache key so deterministic and non-deterministic builds do not share
	// cached binaries.
	Deterministic bool

	swiftPath string
}

// Build compiles src to the given target artefact.
// For TargetMacOSExecutable it returns the path to the built binary.
// For TargetSwiftSource it returns the path to the emitted .swift file.
func (d *Driver) Build(src, outDir string, target Target) (string, error) {
	if d.swiftPath == "" {
		p, err := resolveSwift()
		if err != nil {
			return "", err
		}
		d.swiftPath = p
	}

	srcBytes, err := os.ReadFile(src)
	if err != nil {
		return "", fmt.Errorf("swift build: read %s: %w", src, err)
	}

	className := lower.ClassName(src)

	if target == TargetMacOSExecutable && !d.NoCache {
		cacheKey := d.cacheKey(srcBytes)
		cacheEntry := filepath.Join(d.effectiveCacheDir(), cacheKey)
		if _, err := os.Stat(cacheEntry); err == nil {
			// Cache hit: copy binary to outDir.
			if err := os.MkdirAll(outDir, 0o755); err != nil {
				return "", err
			}
			dest := filepath.Join(outDir, "MochiOut")
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
		return "", fmt.Errorf("swift build: parse: %w", err)
	}

	if errs := types.Check(ast, types.NewEnv(nil)); len(errs) > 0 {
		return "", fmt.Errorf("swift build: typecheck: %w", errs[0])
	}

	prog, err := clower.Lower(ast)
	if err != nil {
		return "", fmt.Errorf("swift build: aotir lower: %w", err)
	}

	colours := colour.Analyse(prog)
	sf, err := lower.Lower(prog, colours, className)
	if err != nil {
		return "", fmt.Errorf("swift build: swift lower: %w", err)
	}

	workDir, err := os.MkdirTemp("", "mochi-swift-*")
	if err != nil {
		return "", err
	}
	defer os.RemoveAll(workDir)

	// Write generated Swift source to Sources/MochiOut/.
	mochiOutDir := filepath.Join(workDir, "Sources", "MochiOut")
	if _, err := emit.Emit(sf, mochiOutDir); err != nil {
		return "", fmt.Errorf("swift build: emit: %w", err)
	}

	if target == TargetSwiftSource {
		if err := os.MkdirAll(outDir, 0o755); err != nil {
			return "", err
		}
		swiftFile := filepath.Join(mochiOutDir, sf.Name+".swift")
		dest := filepath.Join(outDir, sf.Name+".swift")
		if err := copyFile(dest, swiftFile); err != nil {
			return "", err
		}
		return dest, nil
	}

	// Copy MochiRuntime sources into Sources/MochiRuntime/.
	runtimeSrcDir := runtimeSourceDir()
	if runtimeSrcDir == "" {
		return "", fmt.Errorf("swift build: cannot locate MochiRuntime sources")
	}
	runtimeDestDir := filepath.Join(workDir, "Sources", "MochiRuntime")
	if err := copyDir(runtimeDestDir, runtimeSrcDir); err != nil {
		return "", fmt.Errorf("swift build: copy runtime: %w", err)
	}

	// Write Package.swift.
	deterministicPkg := d.Deterministic || os.Getenv("MOCHI_DETERMINISTIC") == "1"
	pkgSwift := generatePackageSwift(deterministicPkg)
	if err := os.WriteFile(filepath.Join(workDir, "Package.swift"), []byte(pkgSwift), 0o644); err != nil {
		return "", fmt.Errorf("swift build: write Package.swift: %w", err)
	}

	// Run swift build -c release (with optional --swift-sdk for static Linux targets).
	buildArgs := []string{"build", "-c", "release"}
	switch target {
	case TargetLinuxStaticX64:
		triple := SDKTripleX64
		if !StaticLinuxSDKAvailable(triple) {
			return "", fmt.Errorf("Swift Static Linux SDK %q not installed; run: swift sdk install ...", triple)
		}
		buildArgs = append(buildArgs, "--swift-sdk", triple)
	case TargetLinuxStaticArm64:
		triple := SDKTripleArm64
		if !StaticLinuxSDKAvailable(triple) {
			return "", fmt.Errorf("Swift Static Linux SDK %q not installed; run: swift sdk install ...", triple)
		}
		buildArgs = append(buildArgs, "--swift-sdk", triple)
	}
	if d.Deterministic || os.Getenv("MOCHI_DETERMINISTIC") == "1" {
		// -gnone removes DWARF. -file-prefix-map remaps the random workDir
		// path in Swift reflection metadata to a canonical placeholder so
		// two builds from different temp dirs produce bit-identical binaries.
		buildArgs = append(buildArgs,
			"-Xswiftc", "-gnone",
			"-Xswiftc", "-file-prefix-map", "-Xswiftc", workDir+"=/_mochi_build_",
		)
	}
	buildCmd := exec.Command(d.swiftPath, buildArgs...)
	buildCmd.Dir = workDir
	buildCmd.Stdout = os.Stderr // forward build output to stderr so tests can see it
	buildCmd.Stderr = os.Stderr
	if d.Deterministic || os.Getenv("MOCHI_DETERMINISTIC") == "1" {
		buildCmd.Env = append(os.Environ(),
			"SWIFTPM_DETERMINISTIC_BUILD=1",
			"SOURCE_DATE_EPOCH=0",
		)
	}
	if err := buildCmd.Run(); err != nil {
		return "", fmt.Errorf("swift build: swift build: %w", err)
	}

	// Find the built binary.
	binPath, err := findBinary(workDir, "MochiOut")
	if err != nil {
		return "", fmt.Errorf("swift build: find binary: %w", err)
	}

	// Copy binary to outDir.
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		return "", err
	}
	dest := filepath.Join(outDir, "MochiOut")
	if err := copyFile(dest, binPath); err != nil {
		return "", err
	}
	if err := os.Chmod(dest, 0o755); err != nil {
		return "", err
	}

	// Cache the binary.
	if !d.NoCache {
		cacheKey := d.cacheKey(srcBytes)
		cacheEntry := filepath.Join(d.effectiveCacheDir(), cacheKey)
		if err := os.MkdirAll(filepath.Dir(cacheEntry), 0o755); err == nil {
			_ = copyFile(cacheEntry, binPath)
		}
	}

	return dest, nil
}

// resolveSwift finds the swift binary.
func resolveSwift() (string, error) {
	if p := os.Getenv("SWIFT_PATH"); p != "" {
		// setup-swift sets SWIFT_PATH to the bin directory; handle both forms.
		if info, err := os.Stat(p); err == nil && info.IsDir() {
			p = filepath.Join(p, "swift")
		}
		return p, nil
	}
	// Check well-known paths first.
	for _, candidate := range []string{"/usr/bin/swift", "/usr/local/bin/swift"} {
		if _, err := os.Stat(candidate); err == nil {
			return candidate, nil
		}
	}
	p, err := exec.LookPath("swift")
	if err != nil {
		return "", fmt.Errorf("swift not found on PATH (set SWIFT_PATH or add swift to PATH): %w", err)
	}
	return p, nil
}

// effectiveCacheDir returns the build cache directory.
func (d *Driver) effectiveCacheDir() string {
	if d.CacheDir != "" {
		return d.CacheDir
	}
	if c := os.Getenv("MOCHI_CACHE_DIR"); c != "" {
		return filepath.Join(c, "swift")
	}
	home, err := os.UserHomeDir()
	if err != nil {
		return os.TempDir()
	}
	return filepath.Join(home, ".cache", "mochi", "swift")
}

// cacheKey computes a SHA-256 cache key from source bytes and swift version.
func (d *Driver) cacheKey(srcBytes []byte) string {
	h := sha256.New()
	h.Write(srcBytes)
	// Include swift path so different swift installs don't share cache.
	h.Write([]byte(d.swiftPath))
	// Include deterministic flag so deterministic and non-deterministic builds
	// do not share cached binaries.
	if d.Deterministic {
		h.Write([]byte{1})
	} else {
		h.Write([]byte{0})
	}
	return fmt.Sprintf("%x", h.Sum(nil))
}

// runtimeSourceDir returns the absolute path to transpiler3/swift/runtime/Sources/MochiRuntime/.
func runtimeSourceDir() string {
	_, thisFile, _, ok := runtime.Caller(0)
	if !ok {
		return ""
	}
	// thisFile: .../transpiler3/swift/build/build.go
	// runtime:  .../transpiler3/swift/runtime/Sources/MochiRuntime/
	dir := filepath.Dir(thisFile)                                                        // .../build
	swiftDir := filepath.Dir(dir)                                                        // .../swift
	p := filepath.Join(swiftDir, "runtime", "Sources", "MochiRuntime")
	if _, err := os.Stat(p); err != nil {
		return ""
	}
	return p
}

// repoRootForBuild returns the absolute repo root by walking up from this source file.
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

// generatePackageSwift returns the Package.swift content for the generated project.
// When deterministic is true it adds linker flags to suppress non-deterministic
// binary metadata (UUID on macOS, build-id on Linux) for reproducible builds.
func generatePackageSwift(deterministic bool) string {
	if !deterministic {
		return `// swift-tools-version: 6.0
import PackageDescription

let package = Package(
    name: "MochiOut",
    platforms: [.macOS(.v13)],
    targets: [
        .executableTarget(
            name: "MochiOut",
            dependencies: ["MochiRuntime"],
            path: "Sources/MochiOut"
        ),
        .target(
            name: "MochiRuntime",
            path: "Sources/MochiRuntime"
        ),
    ]
)
`
	}
	return `// swift-tools-version: 6.0
import PackageDescription

let package = Package(
    name: "MochiOut",
    platforms: [.macOS(.v13)],
    targets: [
        .executableTarget(
            name: "MochiOut",
            dependencies: ["MochiRuntime"],
            path: "Sources/MochiOut",
            linkerSettings: [
                .unsafeFlags(["-Xlinker", "-no_uuid"], .when(platforms: [.macOS])),
                .unsafeFlags(["-Xlinker", "--build-id=none"], .when(platforms: [.linux])),
            ]
        ),
        .target(
            name: "MochiRuntime",
            path: "Sources/MochiRuntime"
        ),
    ]
)
`
}

// findBinary searches for the built binary named name in the swift build output dirs.
func findBinary(workDir, name string) (string, error) {
	// Try .build/release/<name> first (standard layout).
	candidate := filepath.Join(workDir, ".build", "release", name)
	if _, err := os.Stat(candidate); err == nil {
		return candidate, nil
	}

	// Walk .build/ for */release/<name> (platform-specific triple layout).
	buildDir := filepath.Join(workDir, ".build")
	entries, err := os.ReadDir(buildDir)
	if err != nil {
		return "", fmt.Errorf("readdir %s: %w", buildDir, err)
	}
	for _, e := range entries {
		if !e.IsDir() {
			continue
		}
		p := filepath.Join(buildDir, e.Name(), "release", name)
		if _, err := os.Stat(p); err == nil {
			return p, nil
		}
	}

	// Fallback: use find.
	out, err := exec.Command("find", buildDir, "-name", name, "-type", "f").Output()
	if err != nil {
		return "", fmt.Errorf("find binary %s in %s: not found", name, buildDir)
	}
	lines := strings.Split(strings.TrimSpace(string(out)), "\n")
	for _, line := range lines {
		line = strings.TrimSpace(line)
		if line == "" {
			continue
		}
		// Prefer release builds.
		if strings.Contains(line, "/release/") {
			return line, nil
		}
	}
	if len(lines) > 0 && lines[0] != "" {
		return lines[0], nil
	}
	return "", fmt.Errorf("binary %s not found in %s", name, buildDir)
}

// copyFile copies src to dst, creating dst's parent directories as needed.
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

// copyDir copies all regular files from srcDir into dstDir (non-recursive).
func copyDir(dstDir, srcDir string) error {
	if err := os.MkdirAll(dstDir, 0o755); err != nil {
		return err
	}
	entries, err := os.ReadDir(srcDir)
	if err != nil {
		return err
	}
	for _, e := range entries {
		if e.IsDir() {
			continue
		}
		if err := copyFile(filepath.Join(dstDir, e.Name()), filepath.Join(srcDir, e.Name())); err != nil {
			return err
		}
	}
	return nil
}
