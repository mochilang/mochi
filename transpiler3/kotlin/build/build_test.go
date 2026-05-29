package build

import (
	"bytes"
	"os"
	"os/exec"
	"path/filepath"
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

func runKotlinFixture(t *testing.T, srcPath, expectedOutPath string) {
	t.Helper()
	want, err := os.ReadFile(expectedOutPath)
	if err != nil {
		t.Fatalf("read want file %s: %v", expectedOutPath, err)
	}

	outDir := t.TempDir()
	d := &Driver{}
	jarPath, err := d.Build(srcPath, outDir)
	if err != nil {
		t.Fatalf("Build(%s): %v", filepath.Base(srcPath), err)
	}

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
		t.Errorf("stdout mismatch\ngot:  %q\nwant: %q", got, want)
	}
}

func repoRoot(t *testing.T) string {
	t.Helper()
	return repoRootForBuild(t)
}
