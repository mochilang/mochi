package build

import (
	"bytes"
	"crypto/sha256"
	"encoding/hex"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"testing"
)

// resolveJava finds a usable `java` binary for running compiled jars.
func resolveJava(t *testing.T) string {
	t.Helper()
	// Check JAVA_HOME first.
	if jh := os.Getenv("JAVA_HOME"); jh != "" {
		p := filepath.Join(jh, "bin", "java")
		if _, err := os.Stat(p); err == nil {
			return p
		}
	}
	// Check well-known Homebrew openjdk paths (macOS).
	candidates := []string{
		"/opt/homebrew/Cellar/openjdk/26.0.1/libexec/openjdk.jdk/Contents/Home/bin/java",
		"/opt/homebrew/Cellar/openjdk@21/21.0.11/libexec/openjdk.jdk/Contents/Home/bin/java",
		"/opt/homebrew/Cellar/openjdk@17/17.0.19/libexec/openjdk.jdk/Contents/Home/bin/java",
		"/usr/lib/jvm/default-java/bin/java",
	}
	for _, c := range candidates {
		if _, err := os.Stat(c); err == nil {
			return c
		}
	}
	// Fall back to PATH.
	if p, err := exec.LookPath("java"); err == nil {
		return p
	}
	t.Skip("java not found; skipping Kotlin run tests")
	return ""
}

// jarCacheDir returns a persistent cache directory for compiled Kotlin jars.
// Cache key = SHA-256 of the generated .kt source content.
// This avoids re-compiling identical source across test runs.
func jarCacheDir() string {
	home, err := os.UserHomeDir()
	if err != nil {
		return ""
	}
	dir := filepath.Join(home, ".cache", "mochi", "kotlin")
	_ = os.MkdirAll(dir, 0o755)
	return dir
}

// cachedBuild compiles srcPath to a jar, using a persistent SHA-256 cache.
// If a jar for this exact Kotlin source already exists, it is reused.
func cachedBuild(t *testing.T, srcPath, outDir string) string {
	t.Helper()

	// Generate Kotlin source first (without compiling) to get the cache key.
	d := &Driver{}
	ktSrc, err := d.GenerateKt(srcPath)
	if err != nil {
		t.Fatalf("GenerateKt(%s): %v", filepath.Base(srcPath), err)
	}

	// Cache key = SHA-256 of the .kt source.
	h := sha256.Sum256([]byte(ktSrc))
	key := hex.EncodeToString(h[:])

	cacheDir := jarCacheDir()
	var cachedJar string
	if cacheDir != "" {
		cachedJar = filepath.Join(cacheDir, key+".jar")
		if _, err := os.Stat(cachedJar); err == nil {
			// Cache hit: copy to outDir.
			dst := filepath.Join(outDir, "out.jar")
			data, _ := os.ReadFile(cachedJar)
			if err := os.WriteFile(dst, data, 0o644); err == nil {
				return dst
			}
		}
	}

	// Cache miss: compile.
	jarPath, err := d.BuildFromKt(ktSrc, outDir)
	if err != nil {
		t.Fatalf("Build(%s): %v\nGenerated Kotlin:\n%s", filepath.Base(srcPath), err, ktSrc)
	}

	// Store in cache.
	if cacheDir != "" && cachedJar != "" {
		data, _ := os.ReadFile(jarPath)
		_ = os.WriteFile(cachedJar, data, 0o644)
	}

	return jarPath
}

func runKotlinFixture(t *testing.T, srcPath, expectedOutPath string) {
	t.Helper()
	t.Parallel() // run fixture subtests in parallel to amortise kotlinc startup cost

	want, err := os.ReadFile(expectedOutPath)
	if err != nil {
		t.Fatalf("read want file %s: %v", expectedOutPath, err)
	}

	outDir := t.TempDir()
	jarPath := cachedBuild(t, srcPath, outDir)

	javaPath := resolveJava(t)
	cmd := exec.Command(javaPath, "-jar", jarPath)
	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		t.Fatalf("run %s: %v\nstderr: %s", jarPath, err, stderr.String())
	}

	got := stdout.Bytes()
	if !bytes.Equal(got, want) {
		// Generate the Kotlin source for debugging.
		d := &Driver{}
		ktSrc, _ := d.GenerateKt(srcPath)
		t.Errorf("stdout mismatch for %s\ngot:  %q\nwant: %q\n\nGenerated Kotlin:\n%s",
			filepath.Base(srcPath), string(got), string(want), ktSrc)
	}
}

// runFixtureDir runs all .mochi fixtures in a directory.
// Each fixture runs as a parallel subtest.
func runFixtureDir(t *testing.T, fixtureDir string) {
	t.Helper()
	entries, err := os.ReadDir(fixtureDir)
	if err != nil {
		t.Fatalf("ReadDir %s: %v", fixtureDir, err)
	}
	for _, e := range entries {
		if !strings.HasSuffix(e.Name(), ".mochi") {
			continue
		}
		name := strings.TrimSuffix(e.Name(), ".mochi")
		e := e
		t.Run(name, func(t *testing.T) {
			runKotlinFixture(t,
				filepath.Join(fixtureDir, e.Name()),
				filepath.Join(fixtureDir, name+".out"))
		})
	}
}

func repoRoot(t *testing.T) string {
	t.Helper()
	_, thisFile, _, ok := runtime.Caller(0)
	if !ok {
		t.Fatal("cannot determine repo root from runtime.Caller")
	}
	// thisFile = .../transpiler3/kotlin/build/build_test.go
	// repo root is four levels up
	root := filepath.Dir(filepath.Dir(filepath.Dir(filepath.Dir(thisFile))))
	// Validate it looks right.
	if _, err := os.Stat(filepath.Join(root, "go.mod")); err != nil {
		t.Fatalf("repoRoot: expected go.mod at %s", root)
	}
	return root
}

// printKtSource is a test helper that prints the generated Kotlin source for debugging.
func printKtSource(t *testing.T, srcPath string) {
	t.Helper()
	d := &Driver{}
	kt, err := d.GenerateKt(srcPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "GenerateKt error: %v\n", err)
		return
	}
	fmt.Fprintf(os.Stderr, "=== Generated Kotlin for %s ===\n%s\n", filepath.Base(srcPath), kt)
}
