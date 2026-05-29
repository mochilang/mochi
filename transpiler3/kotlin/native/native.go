// Package native drives Kotlin/Native compilation for Linux, macOS, and Windows
// single-binary targets. It generates a minimal Gradle KMP project, invokes
// the Gradle wrapper to link a release executable, and returns the binary path.
package native

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
)

// Target names map to Kotlin/Native konan targets and Gradle task suffixes.
type Target string

const (
	LinuxX64   Target = "kotlin-linux-x64"
	LinuxArm64 Target = "kotlin-linux-arm64"
	MacOSArm64 Target = "kotlin-macos-arm64"
	MacOSX64   Target = "kotlin-macos-x64"
	WindowsX64 Target = "kotlin-windows-x64"
)

// gradleTarget maps a Mochi target name to the Gradle linkRelease task suffix
// (the part after "linkReleaseExecutable").
var gradleTarget = map[Target]string{
	LinuxX64:   "LinuxX64",
	LinuxArm64: "LinuxArm64",
	MacOSArm64: "MacosArm64",
	MacOSX64:   "MacosX64",
	WindowsX64: "MingwX64",
}

// binaryRelPath is the relative path to the compiled binary within the Gradle
// project, keyed by Gradle target suffix.
var binaryRelPath = map[string]string{
	"LinuxX64":   "build/bin/linuxX64/releaseExecutable/mochi.kexe",
	"LinuxArm64": "build/bin/linuxArm64/releaseExecutable/mochi.kexe",
	"MacosArm64": "build/bin/macosArm64/releaseExecutable/mochi.kexe",
	"MacosX64":   "build/bin/macosX64/releaseExecutable/mochi.kexe",
	"MingwX64":   "build/bin/mingwX64/releaseExecutable/mochi.exe",
}

// IsAvailable returns true when a Kotlin/Native toolchain can be found.
// It checks KOTLIN_NATIVE_HOME, KOTLINC_NATIVE_PATH, and PATH.
func IsAvailable() bool {
	if knh := os.Getenv("KOTLIN_NATIVE_HOME"); knh != "" {
		if _, err := os.Stat(filepath.Join(knh, "bin", "kotlinc-native")); err == nil {
			return true
		}
	}
	if knp := os.Getenv("KOTLINC_NATIVE_PATH"); knp != "" {
		if _, err := os.Stat(knp); err == nil {
			return true
		}
	}
	_, err := exec.LookPath("kotlinc-native")
	return err == nil
}

// HostTarget returns the native target for the current OS/arch.
func HostTarget() Target {
	switch runtime.GOOS {
	case "darwin":
		if runtime.GOARCH == "arm64" {
			return MacOSArm64
		}
		return MacOSX64
	case "windows":
		return WindowsX64
	default: // linux
		if runtime.GOARCH == "arm64" {
			return LinuxArm64
		}
		return LinuxX64
	}
}

// Build compiles ktSrc (Kotlin source string) to a native binary for target
// inside projectDir. It generates a Gradle KMP project, runs
// ./gradlew linkReleaseExecutable<Target>, and returns the binary path.
//
// Requires a Kotlin/Native toolchain and Gradle on the PATH (or GRADLE_HOME set).
// Returns an error if the toolchain is not found or compilation fails.
func Build(ktSrc, projectDir string, target Target) (string, error) {
	gt, ok := gradleTarget[target]
	if !ok {
		return "", fmt.Errorf("unsupported target %q", target)
	}

	if err := generateKMPProject(ktSrc, projectDir, gt); err != nil {
		return "", fmt.Errorf("generate KMP project: %w", err)
	}

	gradlew := filepath.Join(projectDir, "gradlew")
	task := "linkReleaseExecutable" + gt
	cmd := exec.Command(gradlew, task, "--no-daemon", "--console=plain")
	cmd.Dir = projectDir
	out, err := cmd.CombinedOutput()
	if err != nil {
		return "", fmt.Errorf("gradle %s: %w\n%s", task, err, string(out))
	}

	binRel, ok := binaryRelPath[gt]
	if !ok {
		return "", fmt.Errorf("no binary path for target %s", gt)
	}
	binPath := filepath.Join(projectDir, binRel)
	if _, err := os.Stat(binPath); err != nil {
		return "", fmt.Errorf("expected binary at %s: %w", binPath, err)
	}
	return binPath, nil
}

func generateKMPProject(ktSrc, projectDir, gt string) error {
	dirs := []string{
		filepath.Join(projectDir, "src", "nativeMain", "kotlin"),
		filepath.Join(projectDir, "gradle", "wrapper"),
	}
	for _, d := range dirs {
		if err := os.MkdirAll(d, 0o755); err != nil {
			return fmt.Errorf("mkdir %s: %w", d, err)
		}
	}

	files := map[string]string{
		"settings.gradle.kts":                      nativeSettingsGradle(),
		"build.gradle.kts":                         nativeBuildGradle(gt),
		"gradle/wrapper/gradle-wrapper.properties": nativeGradleWrapperProperties(),
		"gradlew":                                  nativeGradlewScript(),
		"src/nativeMain/kotlin/Main.kt":            ktSrc,
	}
	for rel, content := range files {
		path := filepath.Join(projectDir, rel)
		mode := os.FileMode(0o644)
		if rel == "gradlew" {
			mode = 0o755
		}
		if err := os.WriteFile(path, []byte(content), mode); err != nil {
			return fmt.Errorf("write %s: %w", rel, err)
		}
	}
	return nil
}

func nativeSettingsGradle() string {
	return `rootProject.name = "mochi"
`
}

func nativeBuildGradle(gt string) string {
	// Map gradle target suffix to KMP target function name.
	targetFn := map[string]string{
		"LinuxX64":   "linuxX64",
		"LinuxArm64": "linuxArm64",
		"MacosArm64": "macosArm64",
		"MacosX64":   "macosX64",
		"MingwX64":   "mingwX64",
	}
	fn := targetFn[gt]
	if fn == "" {
		fn = "linuxX64"
	}
	return fmt.Sprintf(`plugins {
    kotlin("multiplatform") version "2.1.20"
}

repositories {
    mavenCentral()
}

kotlin {
    %s {
        binaries {
            executable {
                entryPoint = "main"
                // Suppress randomised build IDs for reproducible binaries.
                linkerOpts("-Wl,--build-id=none")
            }
        }
    }

    sourceSets {
        nativeMain.dependencies {
            // no external deps; MochiRuntime is inlined for native targets
        }
    }
}
`, fn)
}

func nativeGradleWrapperProperties() string {
	return `distributionBase=GRADLE_USER_HOME
distributionPath=wrapper/dists
distributionUrl=https\://services.gradle.org/distributions/gradle-8.11.1-bin.zip
distributionSha256Sum=f397b287023acdba1e9f6fc5ea72d22dd63669d59ed4a289a29b1a76eee151c6
zipStoreBase=GRADLE_USER_HOME
zipStorePath=wrapper/dists
`
}

func nativeGradlewScript() string {
	return `#!/bin/sh
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
exec gradle --project-dir "$SCRIPT_DIR" "$@"
`
}
