package build

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
)

// Target selects the JVM packaging format.
type Target int

const (
	TargetUberJar Target = iota // fat jar runnable via java -jar
	TargetJvmSource             // .java source files only (debug)
	TargetJLink                 // jlink custom JRE image
	TargetJPackage              // OS-native installer via jpackage
	TargetNativeImage           // GraalVM native-image ahead-of-time binary
)

// Toolchain holds resolved paths to JDK binaries and the detected JDK major version.
type Toolchain struct {
	Java  string // absolute path to java binary
	Javac string // absolute path to javac binary
	Jar   string // absolute path to jar binary
	Major int    // JDK major version (21, 25, ...)
}

// resolveToolchain finds the JDK on PATH or via $JAVA_HOME and returns a
// Toolchain. Returns an error if no JDK 21+ is found.
func resolveToolchain() (*Toolchain, error) {
	// Prefer $JAVA_HOME if set.
	var binDir string
	if jh := os.Getenv("JAVA_HOME"); jh != "" {
		binDir = filepath.Join(jh, "bin")
	}

	javacPath, err := findBinary("javac", binDir)
	if err != nil {
		return nil, fmt.Errorf("javac not found: %w (set JAVA_HOME or add JDK to PATH)", err)
	}

	major, err := javacMajor(javacPath)
	if err != nil {
		return nil, err
	}
	if major < 21 {
		return nil, fmt.Errorf("JDK 21+ required; found JDK %d at %s", major, javacPath)
	}

	dir := filepath.Dir(javacPath)
	tc := &Toolchain{
		Java:  filepath.Join(dir, "java"),
		Javac: javacPath,
		Jar:   filepath.Join(dir, "jar"),
		Major: major,
	}
	return tc, nil
}

// findBinary looks for name in binDir first, then PATH.
func findBinary(name, binDir string) (string, error) {
	if binDir != "" {
		candidate := filepath.Join(binDir, name)
		if _, err := os.Stat(candidate); err == nil {
			return candidate, nil
		}
	}
	return exec.LookPath(name)
}

// javacMajor runs `javac --version` and returns the major version number.
func javacMajor(javacPath string) (int, error) {
	out, err := exec.Command(javacPath, "--version").Output()
	if err != nil {
		return 0, fmt.Errorf("javac --version: %w", err)
	}
	// Output: "javac 21.0.3\n" or "javac 25-ea\n"
	fields := strings.Fields(string(out))
	if len(fields) < 2 {
		return 0, fmt.Errorf("unexpected javac --version output: %q", string(out))
	}
	v := fields[1]
	// Strip EA/preview suffix: "25-ea" -> "25"
	if idx := strings.IndexAny(v, "-+"); idx >= 0 {
		v = v[:idx]
	}
	// Take the major component: "21.0.3" -> "21"
	if idx := strings.Index(v, "."); idx >= 0 {
		v = v[:idx]
	}
	major, err := strconv.Atoi(v)
	if err != nil {
		return 0, fmt.Errorf("cannot parse JDK major version from %q", fields[1])
	}
	return major, nil
}

// Driver is the JVM transpiler pipeline entry point.
type Driver struct {
	// CacheDir overrides the default ~/.cache/mochi/jvm/ location.
	CacheDir string
	tc       *Toolchain
}

// Build compiles src to the given target artefact at out.
func (d *Driver) Build(src, out string, target Target) error {
	if d.tc == nil {
		tc, err := resolveToolchain()
		if err != nil {
			return err
		}
		d.tc = tc
	}
	// Pipeline: stub for Phase 0. Full pipeline added in Phase 1.
	_ = src
	_ = out
	_ = target
	return fmt.Errorf("JVM pipeline not yet implemented (Phase 0 skeleton only)")
}
