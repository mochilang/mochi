package build

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
)

// BuildJLink produces a self-contained JRE image from outJar using jlink.
// The image is written to outDir (a directory, not a jar).
// Returns ErrToolNotFound if jlink or jdeps are not available.
func BuildJLink(tc *Toolchain, outJar, outDir string) error {
	jlinkPath := filepath.Join(filepath.Dir(tc.Javac), "jlink")
	jdepsPath := filepath.Join(filepath.Dir(tc.Javac), "jdeps")
	if _, err := exec.LookPath(jlinkPath); err != nil {
		if _, err2 := exec.LookPath("jlink"); err2 != nil {
			return fmt.Errorf("jlink not found (set JAVA_HOME to a full JDK): %w", err)
		}
		jlinkPath = "jlink"
		jdepsPath = "jdeps"
	}

	// Determine required modules via jdeps.
	jdepsOut, err := exec.Command(jdepsPath, "--ignore-missing-deps", "--list-deps", outJar).Output()
	if err != nil {
		return fmt.Errorf("jdeps: %w", err)
	}
	modules := parseJdepsModules(string(jdepsOut))
	if len(modules) == 0 {
		modules = []string{"java.base"}
	}

	if err := os.MkdirAll(filepath.Dir(outDir), 0o755); err != nil {
		return err
	}
	os.RemoveAll(outDir) // jlink requires the output directory to not exist

	args := []string{
		"--module-path", filepath.Join(filepath.Dir(tc.Javac), "..", "jmods"),
		"--add-modules", joinModules(modules),
		"--compress", "2",
		"--no-header-files",
		"--no-man-pages",
		"--output", outDir,
	}
	cmd := exec.Command(jlinkPath, args...)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("jlink: %w", err)
	}
	return nil
}

func parseJdepsModules(output string) []string {
	var mods []string
	seen := map[string]bool{}
	for _, line := range splitLines(output) {
		line = trimSpace(line)
		if line == "" || line[0] == '/' {
			continue
		}
		if !seen[line] {
			seen[line] = true
			mods = append(mods, line)
		}
	}
	return mods
}

func joinModules(mods []string) string {
	result := ""
	for i, m := range mods {
		if i > 0 {
			result += ","
		}
		result += m
	}
	return result
}

func splitLines(s string) []string {
	var lines []string
	start := 0
	for i := 0; i < len(s); i++ {
		if s[i] == '\n' {
			lines = append(lines, s[start:i])
			start = i + 1
		}
	}
	if start < len(s) {
		lines = append(lines, s[start:])
	}
	return lines
}

func trimSpace(s string) string {
	for len(s) > 0 && (s[0] == ' ' || s[0] == '\t' || s[0] == '\r') {
		s = s[1:]
	}
	for len(s) > 0 && (s[len(s)-1] == ' ' || s[len(s)-1] == '\t' || s[len(s)-1] == '\r') {
		s = s[:len(s)-1]
	}
	return s
}
