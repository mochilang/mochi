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

	args := []string{
		"-jar", outJar,
		"-H:Name=" + filepath.Base(outExe),
		"-H:Path=" + filepath.Dir(outExe),
		"--no-fallback",
		"--initialize-at-build-time",
		"-H:+ReportExceptionStackTraces",
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

