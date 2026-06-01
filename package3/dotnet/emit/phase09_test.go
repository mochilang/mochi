package emit_test

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"mochi/package3/dotnet/emit"
)

// TestPhase9LibraryEmit is the MEP-68 Phase 9 gate for TargetDotnetLibrary emit.
//
// It uses a hand-crafted MochiPackage with two exported functions and verifies:
//   - <PackageId>.csproj exists and contains <PublishAot>true</PublishAot>
//   - MochiExports.cs contains [UnmanagedCallersOnly] for both functions
//   - <PackageId>.nuspec contains the correct id and version
func TestPhase9LibraryEmit(t *testing.T) {
	pkg := &emit.MochiPackage{
		PackageID: "MochiMath",
		Version:   "1.0.0",
		Functions: []emit.MochiFunction{
			{Name: "Add", ParamTypes: []string{"int64", "int64"}, ReturnType: "int64"},
			{Name: "Greet", ParamTypes: []string{"string"}, ReturnType: "string"},
		},
	}

	outDir := t.TempDir()
	if err := emit.EmitNuGetLibrary(pkg, outDir); err != nil {
		t.Fatalf("EmitNuGetLibrary: %v", err)
	}

	pkgDir := filepath.Join(outDir, "MochiMath")

	// Verify .csproj exists and contains PublishAot.
	csprojPath := filepath.Join(pkgDir, "MochiMath.csproj")
	csprojData, err := os.ReadFile(csprojPath)
	if err != nil {
		t.Fatalf("csproj not found: %v", err)
	}
	if !strings.Contains(string(csprojData), "<PublishAot>true</PublishAot>") {
		t.Errorf("csproj missing <PublishAot>true</PublishAot>:\n%s", csprojData)
	}

	// Verify MochiExports.cs contains [UnmanagedCallersOnly] for both functions.
	exportsPath := filepath.Join(pkgDir, "src", "MochiExports.cs")
	exportsData, err := os.ReadFile(exportsPath)
	if err != nil {
		t.Fatalf("MochiExports.cs not found: %v", err)
	}
	exports := string(exportsData)
	if !strings.Contains(exports, "[UnmanagedCallersOnly]") {
		t.Error("MochiExports.cs missing [UnmanagedCallersOnly]")
	}
	if !strings.Contains(exports, "Add") {
		t.Error("MochiExports.cs missing Add function")
	}
	if !strings.Contains(exports, "Greet") {
		t.Error("MochiExports.cs missing Greet function")
	}

	// Verify .nuspec contains correct id and version.
	nuspecPath := filepath.Join(pkgDir, "MochiMath.nuspec")
	nuspecData, err := os.ReadFile(nuspecPath)
	if err != nil {
		t.Fatalf("nuspec not found: %v", err)
	}
	nuspec := string(nuspecData)
	if !strings.Contains(nuspec, "<id>MochiMath</id>") {
		t.Errorf("nuspec missing <id>MochiMath</id>:\n%s", nuspec)
	}
	if !strings.Contains(nuspec, "<version>1.0.0</version>") {
		t.Errorf("nuspec missing <version>1.0.0</version>:\n%s", nuspec)
	}
}
