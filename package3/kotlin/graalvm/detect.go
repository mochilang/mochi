// Package graalvm provides GraalVM native-image detection and compilation helpers.
package graalvm

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"

	bridgeerrors "github.com/mochilang/mochi/package3/kotlin/errors"
)

// ErrGraalVMNotFound is returned when the native-image binary cannot be located.
var ErrGraalVMNotFound = &bridgeerrors.ErrGraalVMNotFound{}

// FindNativeImage searches for the `native-image` binary in this order:
//  1. $GRAALVM_HOME/bin/native-image
//  2. $JAVA_HOME/bin/native-image
//  3. PATH lookup via exec.LookPath
//  4. Platform defaults:
//     Linux:  /usr/lib/jvm/graalvm-ce-21/bin/native-image
//     /usr/lib/jvm/graalvm-jdk-21/bin/native-image
//     macOS:  /Library/Java/JavaVirtualMachines/graalvm-ce-java21-*/Contents/Home/bin/native-image
//     (glob expansion)
//
// Returns (path, version, error).
// Version is parsed from `native-image --version` output, e.g. "native-image 21.0.2 2024-01-16".
// Returns ErrGraalVMNotFound if not found anywhere.
func FindNativeImage() (path, version string, err error) {
	// 1. $GRAALVM_HOME/bin/native-image
	if home := os.Getenv("GRAALVM_HOME"); home != "" {
		candidate := filepath.Join(home, "bin", "native-image")
		if isExecutable(candidate) {
			v, verErr := CheckVersion(candidate)
			if verErr != nil {
				v = ""
			}
			return candidate, v, nil
		}
	}

	// 2. $JAVA_HOME/bin/native-image
	if home := os.Getenv("JAVA_HOME"); home != "" {
		candidate := filepath.Join(home, "bin", "native-image")
		if isExecutable(candidate) {
			v, verErr := CheckVersion(candidate)
			if verErr != nil {
				v = ""
			}
			return candidate, v, nil
		}
	}

	// 3. PATH lookup
	if found, lookErr := exec.LookPath("native-image"); lookErr == nil {
		v, verErr := CheckVersion(found)
		if verErr != nil {
			v = ""
		}
		return found, v, nil
	}

	// 4. Platform defaults
	var defaults []string
	switch runtime.GOOS {
	case "linux":
		defaults = []string{
			"/usr/lib/jvm/graalvm-ce-21/bin/native-image",
			"/usr/lib/jvm/graalvm-jdk-21/bin/native-image",
		}
	case "darwin":
		patterns := []string{
			"/Library/Java/JavaVirtualMachines/graalvm-ce-java21-*/Contents/Home/bin/native-image",
			"/Library/Java/JavaVirtualMachines/graalvm-jdk-21*/Contents/Home/bin/native-image",
		}
		for _, p := range patterns {
			matches, globErr := filepath.Glob(p)
			if globErr == nil {
				defaults = append(defaults, matches...)
			}
		}
	}

	for _, candidate := range defaults {
		if isExecutable(candidate) {
			v, verErr := CheckVersion(candidate)
			if verErr != nil {
				v = ""
			}
			return candidate, v, nil
		}
	}

	return "", "", ErrGraalVMNotFound
}

// CheckVersion runs `native-image --version` and returns the parsed version string.
// For output like "native-image 21.0.2 2024-01-16", it returns "21.0.2".
func CheckVersion(path string) (version string, err error) {
	out, err := exec.Command(path, "--version").Output()
	if err != nil {
		return "", fmt.Errorf("graalvm: running %q --version: %w", path, err)
	}
	return ParseVersion(strings.TrimSpace(string(out))), nil
}

// ParseVersion extracts the version number from native-image --version output.
// Example input: "native-image 21.0.2 2024-01-16"
// Returns "21.0.2".
func ParseVersion(output string) string {
	// Output format: "native-image <version> [date]"
	// or: "GraalVM 22.3.3 Java 17 CE (Java Version 17.0.7+7-jvmci-22.3-b18)"
	// We want the first token that looks like a semver number.
	fields := strings.Fields(output)
	for _, f := range fields {
		// A version field has at least one dot and starts with a digit.
		if len(f) > 0 && f[0] >= '0' && f[0] <= '9' && strings.Contains(f, ".") {
			return f
		}
	}
	// Fall back: return second field if present.
	if len(fields) >= 2 {
		return fields[1]
	}
	return output
}

// isExecutable returns true if the path exists and is an executable file.
func isExecutable(path string) bool {
	info, err := os.Stat(path)
	if err != nil {
		return false
	}
	if info.IsDir() {
		return false
	}
	return info.Mode()&0o111 != 0
}
