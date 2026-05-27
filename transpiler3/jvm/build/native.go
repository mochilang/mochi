package build

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"time"
)

// BuildNativeImage compiles outJar to a native executable using GraalVM native-image.
// The executable is written to outExe.
// Returns an error containing "not found" if native-image is not on PATH.
func BuildNativeImage(outJar, outExe string) error {
	niPath, err := exec.LookPath("native-image")
	if err != nil {
		return fmt.Errorf("native-image not found (install GraalVM or Liberica NIK): %w", err)
	}

	// Determine main class from jar manifest (we know the pattern: dev.mochi.user.XxxMochi).
	mainClass, err := jarMainClass(outJar)
	if err != nil {
		return fmt.Errorf("native-image: %w", err)
	}

	args := []string{
		"-jar", outJar,
		"-H:Name=" + filepath.Base(outExe),
		"-H:Path=" + filepath.Dir(outExe),
		"--no-fallback",
		"--initialize-at-build-time",
		"-H:+ReportExceptionStackTraces",
		"--main-class", mainClass,
	}
	cmd := exec.Command(niPath, args...)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("native-image: %w", err)
	}
	return nil
}

// MeasureStartup runs the executable and returns the wall-clock time until exit.
func MeasureStartup(exe string) (time.Duration, error) {
	start := time.Now()
	cmd := exec.Command(exe)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	err := cmd.Run()
	return time.Since(start), err
}

// jarMainClass reads the Main-Class from a jar's manifest.
func jarMainClass(jarPath string) (string, error) {
	out, err := exec.Command("unzip", "-p", jarPath, "META-INF/MANIFEST.MF").Output()
	if err != nil {
		return "", fmt.Errorf("read manifest: %w", err)
	}
	for _, line := range splitLines(string(out)) {
		line = trimSpace(line)
		const prefix = "Main-Class:"
		if len(line) > len(prefix) && line[:len(prefix)] == prefix {
			return trimSpace(line[len(prefix):]), nil
		}
	}
	return "", fmt.Errorf("Main-Class not found in manifest of %s", jarPath)
}
