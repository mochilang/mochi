// Package build is the end-to-end Kotlin transpiler pipeline driver.
// Driver.Build(src, outDir) returns the path to the compiled .jar file.
package build

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"testing"

	"mochi/parser"
	clower "mochi/transpiler3/c/lower"
	"mochi/transpiler3/kotlin/emit"
	"mochi/transpiler3/kotlin/lower"
	"mochi/types"
)

// resolveKotlinc finds the kotlinc compiler binary.
func resolveKotlinc() (string, error) {
	// Check KOTLINC_PATH env var first.
	if kp := os.Getenv("KOTLINC_PATH"); kp != "" {
		if _, err := os.Stat(kp); err == nil {
			return kp, nil
		}
	}
	// Well-known paths.
	candidates := []string{
		"/usr/local/bin/kotlinc",
		"/opt/homebrew/bin/kotlinc",
	}
	for _, c := range candidates {
		if _, err := os.Stat(c); err == nil {
			return c, nil
		}
	}
	// Fall back to PATH.
	return exec.LookPath("kotlinc")
}

// Driver is the Kotlin transpiler pipeline entry point.
type Driver struct{}

// Build compiles src (a .mochi source file) to an executable .jar in outDir.
// Returns the path to the compiled jar.
func (d *Driver) Build(src, outDir string) (string, error) {
	// Parse.
	ast, err := parser.Parse(src)
	if err != nil {
		return "", fmt.Errorf("kotlin build: parse: %w", err)
	}

	// Type-check.
	if errs := types.Check(ast, types.NewEnv(nil)); len(errs) > 0 {
		return "", fmt.Errorf("kotlin build: typecheck: %w", errs[0])
	}

	// Lower to aotir.
	prog, err := clower.Lower(ast)
	if err != nil {
		return "", fmt.Errorf("kotlin build: aotir lower: %w", err)
	}

	// Lower to ktree.
	sf, err := lower.Lower(prog)
	if err != nil {
		return "", fmt.Errorf("kotlin build: kotlin lower: %w", err)
	}

	// Emit Kotlin source.
	workDir, err := os.MkdirTemp("", "mochi-kotlin-*")
	if err != nil {
		return "", err
	}
	defer os.RemoveAll(workDir)

	ktFile, err := emit.Emit(sf, workDir)
	if err != nil {
		return "", fmt.Errorf("kotlin build: emit: %w", err)
	}

	// Compile with kotlinc.
	kotlincPath, err := resolveKotlinc()
	if err != nil {
		return "", fmt.Errorf("kotlin build: kotlinc not found: %w", err)
	}

	jarPath := filepath.Join(outDir, "out.jar")
	cmd := exec.Command(kotlincPath, ktFile, "-include-runtime", "-d", jarPath)
	out, kerr := cmd.CombinedOutput()
	if kerr != nil {
		return "", fmt.Errorf("kotlin build: kotlinc: %w\n%s", kerr, string(out))
	}

	return jarPath, nil
}

// repoRootForBuild resolves the repository root from the location of this Go
// source file so paths work regardless of the working directory at test time.
func repoRootForBuild(t *testing.T) string {
	t.Helper()
	_, thisFile, _, ok := runtime.Caller(0)
	if ok {
		// thisFile is .../transpiler3/kotlin/build/build.go
		// repo root is four dirs up
		return filepath.Dir(filepath.Dir(filepath.Dir(filepath.Dir(thisFile))))
	}
	// Fallback: return empty string (tests will fail with a clear error).
	t.Fatal("cannot determine repo root from runtime.Caller")
	return ""
}
