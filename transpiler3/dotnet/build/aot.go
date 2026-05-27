package build

import (
	"fmt"
	"os/exec"
	"runtime"
	"strings"
)

// hostRID returns the .NET runtime identifier for the host OS and arch.
func hostRID() string {
	var os, arch string
	switch runtime.GOOS {
	case "linux":
		os = "linux"
	case "darwin":
		os = "osx"
	case "windows":
		os = "win"
	default:
		os = runtime.GOOS
	}
	switch runtime.GOARCH {
	case "amd64":
		arch = "x64"
	case "arm64":
		arch = "arm64"
	case "386":
		arch = "x86"
	default:
		arch = runtime.GOARCH
	}
	return os + "-" + arch
}

// generateAotCsproj returns .csproj XML for NativeAOT publish.
// runtimeCsproj is the absolute path to Mochi.Runtime.csproj.
func generateAotCsproj(className, tfm, rid, runtimeCsproj string) string {
	projRef := ""
	if runtimeCsproj != "" {
		projRef = fmt.Sprintf(`
  <ItemGroup>
    <ProjectReference Include="%s" />
  </ItemGroup>`, runtimeCsproj)
	}
	// On macOS, homebrew libraries may not be in the default linker search path.
	// Add /opt/homebrew/lib if it exists (OpenSSL, brotli, etc. are needed by the .NET TLS stack).
	macLinkerExtra := ""
	if runtime.GOOS == "darwin" {
		macLinkerExtra = `
  <ItemGroup>
    <LinkerArg Include="-L/opt/homebrew/lib" />
  </ItemGroup>`
	}
	return fmt.Sprintf(`<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>%s</TargetFramework>
    <RuntimeIdentifier>%s</RuntimeIdentifier>
    <AssemblyName>%s</AssemblyName>
    <Nullable>enable</Nullable>
    <ImplicitUsings>enable</ImplicitUsings>
    <LangVersion>latest</LangVersion>
    <PublishAot>true</PublishAot>
    <InvariantGlobalization>true</InvariantGlobalization>
    <Deterministic>true</Deterministic>
    <IlcMaxParallelism>1</IlcMaxParallelism>
    <TreatWarningsAsErrors>false</TreatWarningsAsErrors>
    <Optimize>true</Optimize>
  </PropertyGroup>%s%s
</Project>
`, tfm, rid, className, projRef, macLinkerExtra)
}

// packAot publishes a NativeAOT ahead-of-time compiled binary to outDir.
// Produces a native executable in outDir/className (or className.exe on Windows).
func packAot(dotnetPath, projDir, outDir, tfm string) error {
	rid := hostRID()
	args := []string{
		"publish", projDir,
		"-r", rid,
		"-c", "Release",
		"--output", outDir,
		"--nologo",
		"-v", "minimal",
	}
	out, err := exec.Command(dotnetPath, args...).CombinedOutput()
	if err != nil {
		return fmt.Errorf("dotnet publish (aot): %w\n%s", err, strings.TrimSpace(string(out)))
	}
	return nil
}
