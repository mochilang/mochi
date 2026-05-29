package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase26TargetTruffleNative covers TargetTruffleNative: emit the script
// and a native_build.sh that drives `native-image --language:ruby` against
// the bundled script. native-image is not invoked since GraalVM is typically
// unavailable in CI.
func TestPhase26TargetTruffleNative(t *testing.T) {
	if _, err := resolveToolchain(); err != nil {
		t.Skipf("ruby toolchain not available: %v", err)
	}

	srcDir := t.TempDir()
	src := filepath.Join(srcDir, "hello_truffle.mochi")
	if err := os.WriteFile(src, []byte("print(\"hi from truffle\")\n"), 0o644); err != nil {
		t.Fatalf("write src: %v", err)
	}

	out := t.TempDir()
	d := &Driver{CacheDir: t.TempDir()}
	if err := d.Build(src, out, TargetTruffleNative); err != nil {
		t.Fatalf("Build TargetTruffleNative: %v", err)
	}

	scriptPath := filepath.Join(out, "hello_truffle.rb")
	buildPath := filepath.Join(out, "native_build.sh")
	for _, p := range []string{scriptPath, buildPath} {
		if _, err := os.Stat(p); err != nil {
			t.Fatalf("missing artefact %s: %v", p, err)
		}
	}

	scriptBytes, _ := os.ReadFile(scriptPath)
	if !strings.Contains(string(scriptBytes), "hi from truffle") {
		t.Fatalf("script missing literal:\n%s", scriptBytes)
	}

	buildBytes, _ := os.ReadFile(buildPath)
	buildStr := string(buildBytes)
	for _, want := range []string{
		"#!/usr/bin/env bash",
		"set -euo pipefail",
		"--language:ruby",
		"--no-fallback",
		"--initialize-at-build-time",
		`-o "hello_truffle"`,
		`"hello_truffle.rb"`,
		"MOCHI_GRAALVM_HOME",
		"GRAALVM_HOME",
		"native-image",
		`echo "MOCHI_GRAALVM_HOME or GRAALVM_HOME must be set"`,
		`echo "native-image not found at $NATIVE_IMAGE"`,
	} {
		if !strings.Contains(buildStr, want) {
			t.Fatalf("native_build.sh missing %q:\n%s", want, buildStr)
		}
	}

	info, err := os.Stat(buildPath)
	if err != nil {
		t.Fatalf("stat native_build.sh: %v", err)
	}
	if info.Mode().Perm()&0o111 == 0 {
		t.Fatalf("native_build.sh is not executable: mode=%v", info.Mode())
	}

	checkBashSyntax(t, buildPath)
}
