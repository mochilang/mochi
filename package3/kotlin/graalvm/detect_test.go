package graalvm

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestFindNativeImage_GraalVMHome(t *testing.T) {
	// Create a temp dir with a fake native-image binary.
	dir := t.TempDir()
	binDir := filepath.Join(dir, "bin")
	if err := os.MkdirAll(binDir, 0o755); err != nil {
		t.Fatal(err)
	}
	fakeBin := filepath.Join(binDir, "native-image")
	script := "#!/bin/sh\necho 'native-image 21.0.2 2024-01-16'\n"
	if err := os.WriteFile(fakeBin, []byte(script), 0o755); err != nil {
		t.Fatal(err)
	}

	t.Setenv("GRAALVM_HOME", dir)
	t.Setenv("JAVA_HOME", "") // make sure JAVA_HOME doesn't interfere

	path, version, err := FindNativeImage()
	if err != nil {
		t.Fatalf("FindNativeImage: unexpected error: %v", err)
	}
	if path != fakeBin {
		t.Errorf("path: got %q, want %q", path, fakeBin)
	}
	if version != "21.0.2" {
		t.Errorf("version: got %q, want %q", version, "21.0.2")
	}
}

func TestFindNativeImage_JavaHome(t *testing.T) {
	// Create a temp dir with a fake native-image binary.
	dir := t.TempDir()
	binDir := filepath.Join(dir, "bin")
	if err := os.MkdirAll(binDir, 0o755); err != nil {
		t.Fatal(err)
	}
	fakeBin := filepath.Join(binDir, "native-image")
	script := "#!/bin/sh\necho 'native-image 21.0.3 2024-02-01'\n"
	if err := os.WriteFile(fakeBin, []byte(script), 0o755); err != nil {
		t.Fatal(err)
	}

	t.Setenv("GRAALVM_HOME", "") // ensure GRAALVM_HOME not set
	t.Setenv("JAVA_HOME", dir)

	path, version, err := FindNativeImage()
	if err != nil {
		t.Fatalf("FindNativeImage: unexpected error: %v", err)
	}
	if path != fakeBin {
		t.Errorf("path: got %q, want %q", path, fakeBin)
	}
	if version != "21.0.3" {
		t.Errorf("version: got %q, want %q", version, "21.0.3")
	}
}

func TestFindNativeImage_NotFound(t *testing.T) {
	t.Setenv("GRAALVM_HOME", "")
	t.Setenv("JAVA_HOME", "")
	// We can't control PATH easily, but we can at least verify the function returns
	// an error when neither env var is set and native-image is likely not on PATH.
	// If native-image happens to be on the test machine this test may pass anyway.
	// We just ensure no panic occurs.
	_, _, err := FindNativeImage()
	// If native-image is found on PATH, err will be nil; that's acceptable.
	// If not found, err should be ErrGraalVMNotFound.
	if err != nil {
		if !strings.Contains(err.Error(), "GraalVM native-image not found") {
			t.Errorf("expected ErrGraalVMNotFound, got: %v", err)
		}
	}
}

func TestCheckVersion_ParseVersion(t *testing.T) {
	// Create a fake script that prints a version string.
	dir := t.TempDir()
	fakeBin := filepath.Join(dir, "native-image")
	script := "#!/bin/sh\necho 'native-image 21.0.2 2024-01-16'\n"
	if err := os.WriteFile(fakeBin, []byte(script), 0o755); err != nil {
		t.Fatal(err)
	}

	version, err := CheckVersion(fakeBin)
	if err != nil {
		t.Fatalf("CheckVersion: unexpected error: %v", err)
	}
	if version != "21.0.2" {
		t.Errorf("version: got %q, want %q", version, "21.0.2")
	}
}

func TestParseVersion(t *testing.T) {
	tests := []struct {
		input string
		want  string
	}{
		{"native-image 21.0.2 2024-01-16", "21.0.2"},
		{"native-image 22.3.3 2023-07-18", "22.3.3"},
		{"GraalVM 22.3.3 Java 17 CE (Java Version 17.0.7+7-jvmci-22.3-b18)", "22.3.3"},
		{"native-image 17.0.7 2023-04-18\nGraalVM Runtime Environment ...", "17.0.7"},
	}
	for _, tt := range tests {
		got := ParseVersion(tt.input)
		if got != tt.want {
			t.Errorf("ParseVersion(%q) = %q, want %q", tt.input, got, tt.want)
		}
	}
}
