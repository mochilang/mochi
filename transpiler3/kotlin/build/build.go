// Package build is the end-to-end Kotlin transpiler pipeline driver.
// Driver.GenerateKt(src) returns the generated Kotlin source string.
// Driver.BuildFromKt(ktSrc, outDir) compiles it to a jar.
// Driver.Build(src, outDir) is the combined convenience method.
package build

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"

	"mochi/parser"
	clower "mochi/transpiler3/c/lower"
	"mochi/transpiler3/kotlin/lower"
	"mochi/types"
)

// resolveKotlinc finds the kotlinc compiler binary.
func resolveKotlinc() (string, error) {
	if kp := os.Getenv("KOTLINC_PATH"); kp != "" {
		if _, err := os.Stat(kp); err == nil {
			return kp, nil
		}
	}
	candidates := []string{
		"/usr/local/bin/kotlinc",
		"/opt/homebrew/bin/kotlinc",
	}
	for _, c := range candidates {
		if _, err := os.Stat(c); err == nil {
			return c, nil
		}
	}
	return exec.LookPath("kotlinc")
}

// Driver is the Kotlin transpiler pipeline entry point.
type Driver struct{}

// GenerateKt parses src (a .mochi file) and returns the generated Kotlin source.
// Does NOT invoke the Kotlin compiler — useful for caching and debugging.
func (d *Driver) GenerateKt(src string) (string, error) {
	ast, err := parser.Parse(src)
	if err != nil {
		return "", fmt.Errorf("parse: %w", err)
	}
	if errs := types.Check(ast, types.NewEnv(nil)); len(errs) > 0 {
		return "", fmt.Errorf("typecheck: %w", errs[0])
	}
	prog, err := clower.Lower(ast)
	if err != nil {
		return "", fmt.Errorf("aotir lower: %w", err)
	}
	sf, err := lower.Lower(prog)
	if err != nil {
		return "", fmt.Errorf("kotlin lower: %w", err)
	}
	return sf.KtSource(), nil
}

// BuildFromKt compiles a Kotlin source string to a runnable .jar in outDir.
// Returns the path to the compiled jar.
func (d *Driver) BuildFromKt(ktSrc, outDir string) (string, error) {
	kotlincPath, err := resolveKotlinc()
	if err != nil {
		return "", fmt.Errorf("kotlinc not found: %w", err)
	}

	// Write source to a temp file inside outDir (not a separate TempDir,
	// so the caller can still see it if needed).
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		return "", err
	}
	ktFile := filepath.Join(outDir, "Main.kt")
	if err := os.WriteFile(ktFile, []byte(ktSrc), 0o644); err != nil {
		return "", fmt.Errorf("write Main.kt: %w", err)
	}

	jarPath := filepath.Join(outDir, "out.jar")
	cmd := exec.Command(kotlincPath, ktFile, "-include-runtime", "-d", jarPath)
	out, kerr := cmd.CombinedOutput()
	if kerr != nil {
		// Include the first few lines of the Kotlin source for context.
		lines := strings.SplitN(ktSrc, "\n", 20)
		return "", fmt.Errorf("kotlinc: %w\n%s\n\nFirst 20 lines of generated Kotlin:\n%s",
			kerr, string(out), strings.Join(lines, "\n"))
	}
	return jarPath, nil
}

// Build is the combined convenience method: GenerateKt + BuildFromKt.
func (d *Driver) Build(src, outDir string) (string, error) {
	kt, err := d.GenerateKt(src)
	if err != nil {
		return "", err
	}
	return d.BuildFromKt(kt, outDir)
}

// EmitToDir writes the generated .kt file to dir and returns the file path.
// Useful for inspection and CI artefacts.
func (d *Driver) EmitToDir(src, dir string) (string, error) {
	kt, err := d.GenerateKt(src)
	if err != nil {
		return "", err
	}
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return "", err
	}
	p := filepath.Join(dir, "Main.kt")
	return p, os.WriteFile(p, []byte(kt), 0o644)
}
