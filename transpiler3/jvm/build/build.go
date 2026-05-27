package build

import (
	"crypto/sha256"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strconv"
	"strings"

	"mochi/parser"
	clower "mochi/transpiler3/c/lower"
	"mochi/transpiler3/jvm/emit"
	"mochi/transpiler3/jvm/lower"
	"mochi/types"
)

// Target selects the JVM packaging format.
type Target int

const (
	TargetUberJar     Target = iota // fat jar runnable via java -jar
	TargetJvmSource                 // .java source files only (debug)
	TargetJLink                     // jlink custom JRE image
	TargetJPackage                  // OS-native installer via jpackage
	TargetNativeImage               // GraalVM native-image ahead-of-time binary
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
	CacheDir        string
	tc              *Toolchain
	runtimeJarSHA   string // memoised SHA-256 of runtime jar
}

// runtimeJarPath returns the path to the pre-built mochi-runtime jar.
// It resolves the repo root from the location of this Go source file so the
// path is correct regardless of the working directory at test time.
func runtimeJarPath() string {
	_, thisFile, _, ok := runtime.Caller(0)
	if ok {
		// thisFile is .../transpiler3/jvm/build/build.go
		// repo root is three dirs up
		repoRoot := filepath.Dir(filepath.Dir(filepath.Dir(filepath.Dir(thisFile))))
		for _, name := range []string{"mochi-runtime-0.14.0.jar", "mochi-runtime-0.10.0-SNAPSHOT.jar"} {
			jarPath := filepath.Join(repoRoot, "transpiler3", "jvm", "runtime", "target", name)
			if _, err := os.Stat(jarPath); err == nil {
				return jarPath
			}
		}
	}
	// Fallback: relative from CWD (e.g. running from repo root directly)
	candidates := []string{
		"transpiler3/jvm/runtime/target/mochi-runtime-0.14.0.jar",
		"transpiler3/jvm/runtime/target/mochi-runtime-0.10.0-SNAPSHOT.jar",
	}
	for _, c := range candidates {
		if _, err := os.Stat(c); err == nil {
			abs, _ := filepath.Abs(c)
			return abs
		}
	}
	return ""
}

func (d *Driver) runtimeJar() string {
	return runtimeJarPath()
}

func (d *Driver) runtimeJarSHA256() string {
	if d.runtimeJarSHA != "" {
		return d.runtimeJarSHA
	}
	rj := d.runtimeJar()
	if rj == "" {
		return ""
	}
	f, err := os.Open(rj)
	if err != nil {
		return ""
	}
	defer f.Close()
	h := sha256.New()
	io.Copy(h, f) //nolint:errcheck
	d.runtimeJarSHA = fmt.Sprintf("%x", h.Sum(nil))
	return d.runtimeJarSHA
}

func (d *Driver) effectiveCacheDir() string {
	if d.CacheDir != "" {
		return d.CacheDir
	}
	home, _ := os.UserHomeDir()
	return filepath.Join(home, ".cache", "mochi", "jvm")
}

// cacheKey computes a SHA-256-based cache key from source bytes + JDK version + runtime jar SHA.
func (d *Driver) cacheKey(srcBytes []byte) string {
	jdkVersion := fmt.Sprintf("jdk%d", d.tc.Major)
	runtimeSHA := d.runtimeJarSHA256()

	h := sha256.New()
	h.Write(srcBytes)
	h.Write([]byte(jdkVersion))
	h.Write([]byte(runtimeSHA))
	return fmt.Sprintf("%x", h.Sum(nil))
}

// Build compiles src to the given target artefact at out.
func (d *Driver) Build(src, out string, target Target) error {
	// Resolve toolchain on first call.
	if d.tc == nil {
		tc, err := resolveToolchain()
		if err != nil {
			return err
		}
		d.tc = tc
	}

	// Read source.
	srcBytes, err := os.ReadFile(src)
	if err != nil {
		return fmt.Errorf("jvm build: read %s: %w", src, err)
	}

	// Check cache.
	cacheKey := d.cacheKey(srcBytes)
	cacheEntry := filepath.Join(d.effectiveCacheDir(), cacheKey+".jar")
	if _, err := os.Stat(cacheEntry); err == nil {
		return copyFile(out, cacheEntry)
	}

	// Parse.
	ast, err := parser.Parse(src)
	if err != nil {
		return fmt.Errorf("jvm build: parse: %w", err)
	}

	// Type-check.
	if errs := types.Check(ast, types.NewEnv(nil)); len(errs) > 0 {
		return fmt.Errorf("jvm build: typecheck: %w", errs[0])
	}

	// Lower to aotir.
	prog, err := clower.Lower(ast)
	if err != nil {
		return fmt.Errorf("jvm build: aotir lower: %w", err)
	}

	// Lower to javasrc (returns one CU per record type + one for main class).
	className := lower.ClassName(src)
	cus, err := lower.Lower(prog, className)
	if err != nil {
		return fmt.Errorf("jvm build: jvm lower: %w", err)
	}

	// Emit Java source.
	workDir, err := os.MkdirTemp("", "mochi-jvm-*")
	if err != nil {
		return err
	}
	defer os.RemoveAll(workDir)

	srcDir := filepath.Join(workDir, "src")
	classDir := filepath.Join(workDir, "classes")
	os.MkdirAll(srcDir, 0o755)   //nolint:errcheck
	os.MkdirAll(classDir, 0o755) //nolint:errcheck

	var javaFiles []string
	for _, cu := range cus {
		files, err := emit.Emit(cu, srcDir)
		if err != nil {
			return fmt.Errorf("jvm build: emit: %w", err)
		}
		javaFiles = append(javaFiles, files...)
	}

	// jvm-source target: emit .java files only, no compilation.
	if target == TargetJvmSource {
		if err := os.MkdirAll(out, 0o755); err != nil {
			return err
		}
		for _, f := range javaFiles {
			dst := filepath.Join(out, filepath.Base(f))
			if err := copyFile(dst, f); err != nil {
				return fmt.Errorf("jvm build: copy source: %w", err)
			}
		}
		return nil
	}

	// Compile with javac. Pass the runtime jar on the classpath so
	// dev.mochi.runtime.io.IO is visible at compile time.
	flags := []string{"--release", "21", "-Xlint:all"}
	if rj := d.runtimeJar(); rj != "" {
		flags = append(flags, "-classpath", rj)
	}
	if err := emit.Compile(d.tc.Javac, javaFiles, classDir, flags); err != nil {
		return fmt.Errorf("jvm build: javac: %w", err)
	}

	// Package.
	mainClass := "dev.mochi.user." + className
	tmpJar := filepath.Join(workDir, "out.jar")
	if err := PackUberJar(classDir, d.runtimeJar(), tmpJar, mainClass); err != nil {
		return fmt.Errorf("jvm build: uberjar: %w", err)
	}

	// Write to output.
	if err := os.MkdirAll(filepath.Dir(out), 0o755); err != nil {
		return err
	}
	if err := copyFile(out, tmpJar); err != nil {
		return err
	}

	// Populate cache (best-effort): copy out -> cacheEntry.
	cacheDir := d.effectiveCacheDir()
	os.MkdirAll(cacheDir, 0o755)    //nolint:errcheck
	copyFile(cacheEntry, out)        //nolint:errcheck

	return nil
}

// copyFile copies src to dst, creating dst's parent directories as needed.
func copyFile(dst, src string) error {
	if err := os.MkdirAll(filepath.Dir(dst), 0o755); err != nil {
		return err
	}
	in, err := os.Open(src)
	if err != nil {
		return err
	}
	defer in.Close()
	out, err := os.Create(dst)
	if err != nil {
		return err
	}
	defer out.Close()
	_, err = io.Copy(out, in)
	return err
}
