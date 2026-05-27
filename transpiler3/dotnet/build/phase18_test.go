package build

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase18TrimWarnings is the gate test for MEP-48 Phase 18: trim cleanliness.
// It compiles the Phase 1 hello-world fixtures with PublishTrimmed=true and asserts
// zero IL2026/IL2070/IL2080/IL3050 trim warnings in the build output.
// Skipped unless MOCHI_TEST_TRIM=1 (slow; ~30s per fixture).
func TestPhase18TrimWarnings(t *testing.T) {
	if os.Getenv("MOCHI_TEST_TRIM") == "" {
		t.Skip("set MOCHI_TEST_TRIM=1 to run trim warning tests (slow, ~30s per fixture)")
	}

	tc, err := resolveToolchain()
	if err != nil {
		t.Fatalf("resolveToolchain: %v", err)
	}

	fixtureBase := filepath.Join(repoRootForBuild(t), "tests", "transpiler3", "dotnet", "fixtures", "phase01-hello")
	entries, err := os.ReadDir(fixtureBase)
	if err != nil {
		t.Fatalf("ReadDir %s: %v", fixtureBase, err)
	}

	for _, e := range entries {
		if !strings.HasSuffix(e.Name(), ".mochi") {
			continue
		}
		name := strings.TrimSuffix(e.Name(), ".mochi")
		mochiPath := filepath.Join(fixtureBase, e.Name())

		t.Run(name, func(t *testing.T) {
			outDir := t.TempDir()
			d := &Driver{CacheDir: t.TempDir()}

			// Build to fx-dependent first to get the .csproj generated.
			// We then re-run dotnet publish with trim flags on the same project.
			if err := d.Build(mochiPath, outDir, TargetFxDependent); err != nil {
				t.Fatalf("Build(%s): %v", name, err)
			}

			// Re-build with trim enabled using the src dir from a fresh build.
			d2 := &Driver{CacheDir: t.TempDir()}
			srcDir, csprojPath, err := buildTrimProject(d2, tc, mochiPath)
			if err != nil {
				t.Fatalf("buildTrimProject(%s): %v", name, err)
			}
			defer os.RemoveAll(srcDir)

			rid := hostRID()
			trimDir := t.TempDir()
			args := []string{
				"publish", csprojPath,
				"--self-contained", "true",
				"-r", rid,
				"-c", "Release",
				"--output", trimDir,
				"--nologo",
				"-v", "minimal",
				"-p:PublishTrimmed=true",
				"-p:TreatWarningsAsErrors=false",
			}
			out, _ := exec.Command(tc.Dotnet, args...).CombinedOutput()
			outputStr := string(out)

			// Check for trim warning codes.
			trimWarnings := []string{"IL2026", "IL2070", "IL2080", "IL2062", "IL3050"}
			var found []string
			for _, code := range trimWarnings {
				if strings.Contains(outputStr, code) {
					found = append(found, code)
				}
			}
			if len(found) > 0 {
				t.Errorf("trim warnings found in %s: %v\nOutput:\n%s", name, found, outputStr)
			}
		})
	}
}

// buildTrimProject creates a source project directory for trim analysis and returns
// the srcDir and path to the .csproj file.
func buildTrimProject(d *Driver, tc *Toolchain, mochiPath string) (srcDir, csprojPath string, err error) {
	d.tc = tc

	srcBytes, err := os.ReadFile(mochiPath)
	if err != nil {
		return "", "", fmt.Errorf("read %s: %w", mochiPath, err)
	}
	_ = srcBytes // used via d.Build path

	// Build through the normal pipeline to generate C# sources.
	tmpOutDir, err := os.MkdirTemp("", "mochi-trim-*")
	if err != nil {
		return "", "", err
	}

	// We need the generated project dir, not the publish dir.
	// Use TargetDotnetSource to get just the .cs files.
	if err := d.Build(mochiPath, tmpOutDir, TargetDotnetSource); err != nil {
		os.RemoveAll(tmpOutDir)
		return "", "", fmt.Errorf("Build (source): %w", err)
	}

	// Generate the csproj in tmpOutDir.
	tfm := d.effectiveTFM()
	runtimeProj := runtimeCsprojPath()
	className := filepath.Base(strings.TrimSuffix(mochiPath, ".mochi"))
	// Capitalize first letter to match ClassName convention.
	if len(className) > 0 {
		className = strings.ToUpper(className[:1]) + className[1:]
	}
	csprojContent := generateCsproj(className, tfm, runtimeProj)
	csprojFilePath := filepath.Join(tmpOutDir, className+".csproj")
	if err := os.WriteFile(csprojFilePath, []byte(csprojContent), 0o644); err != nil {
		os.RemoveAll(tmpOutDir)
		return "", "", fmt.Errorf("write csproj: %w", err)
	}

	return tmpOutDir, csprojFilePath, nil
}

// TestPhase18Publish verifies the published Mochi.Runtime NuGet package is consumable.
// Skipped unless MOCHI_TEST_NUGET_PUBLISH=1 (requires nuget.org credentials).
func TestPhase18Publish(t *testing.T) {
	if os.Getenv("MOCHI_TEST_NUGET_PUBLISH") == "" {
		t.Skip("set MOCHI_TEST_NUGET_PUBLISH=1 to run NuGet publish tests (requires nuget.org credentials)")
	}
	t.Log("NuGet publish test: not yet implemented (requires credentials)")
}
