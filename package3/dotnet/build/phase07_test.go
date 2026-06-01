package build_test

import (
	"context"
	"os/exec"
	"runtime"
	"strings"
	"testing"

	"mochi/package3/dotnet/build"
)

// TestPhase7Build is the MEP-68 Phase 7 gate for the .NET NuGet bridge.
//
// It skips unless `dotnet` is on $PATH and a .NET SDK 8+ is installed.
// The test exercises Driver.Build for Newtonsoft.Json 13.0.3 end-to-end:
//
//   - Workspace is created under t.TempDir()
//   - dotnet publish with /p:PublishAot=true /p:NativeLib=Static is invoked
//   - The returned PackageLink.LinkerFlags must contain the expected lib name
func TestPhase7Build(t *testing.T) {
	dotnetPath, err := exec.LookPath("dotnet")
	if err != nil {
		t.Skip("dotnet not on $PATH; skipping Phase 7 build gate")
	}

	// Verify SDK 8+ is available.
	out, err := exec.Command(dotnetPath, "--version").Output()
	if err != nil {
		t.Skipf("dotnet --version failed: %v; skipping", err)
	}
	ver := strings.TrimSpace(string(out))
	if len(ver) == 0 || (ver[0] < '8') {
		t.Skipf("dotnet SDK version %q is too old (need 8+); skipping", ver)
	}

	workDir := t.TempDir()
	d := build.NewDriver(workDir)

	spec := build.BuildSpec{
		PackageID:          "Newtonsoft.Json",
		Version:            "13.0.3",
		RuntimeIdentifiers: []string{hostRID()},
		CacheDir:           t.TempDir(),
		Verbose:            false,
	}

	link, err := d.Build(context.Background(), spec)
	if err != nil {
		// dotnet publish can fail due to NuGet restore (network) or NativeAOT toolchain.
		// Treat those as skip conditions rather than test failures.
		msg := err.Error()
		if strings.Contains(msg, "restore") || strings.Contains(msg, "toolchain") ||
			strings.Contains(msg, "NativeAOT") || strings.Contains(msg, "Unable to load") ||
			strings.Contains(msg, "network") {
			t.Skipf("dotnet publish skipped due to environment: %v", err)
		}
		t.Fatalf("Driver.Build: %v", err)
	}
	if link == nil {
		t.Fatal("Driver.Build returned nil PackageLink")
	}

	rid := hostRID()
	libPath, ok := link.StaticLibs[rid]
	if !ok {
		t.Errorf("PackageLink.StaticLibs missing %s entry", rid)
	} else if libPath == "" {
		t.Errorf("PackageLink.StaticLibs[%s] is empty", rid)
	}

	// Verify that LinkerFlags contains the expected cargo:rustc-link-lib= flag.
	found := false
	for _, f := range link.LinkerFlags {
		if strings.Contains(f, "newtonsoft_json_dotnet_wrap") {
			found = true
			break
		}
	}
	if !found {
		t.Errorf("LinkerFlags %v does not contain newtonsoft_json_dotnet_wrap", link.LinkerFlags)
	}
}

// hostRID returns the runtime identifier for the current host platform.
func hostRID() string {
	switch runtime.GOOS {
	case "darwin":
		if runtime.GOARCH == "arm64" {
			return "osx-arm64"
		}
		return "osx-x64"
	case "windows":
		if runtime.GOARCH == "arm64" {
			return "win-arm64"
		}
		return "win-x64"
	default: // linux
		if runtime.GOARCH == "arm64" {
			return "linux-arm64"
		}
		return "linux-x64"
	}
}
