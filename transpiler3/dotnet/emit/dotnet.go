package emit

import (
	"fmt"
	"os/exec"
	"strings"
)

// Compile invokes `dotnet build` to compile csFiles into outDir.
// tfm is the target framework moniker, e.g. "net8.0".
// dotnetPath is the absolute path to the dotnet binary.
func Compile(csFiles []string, outDir string, tfm string, dotnetPath string) error {
	_ = csFiles // dotnet build discovers files via the .csproj; csFiles is informational.
	args := []string{
		"build",
		"--output", outDir,
		"--framework", tfm,
		"--nologo",
		"-v", "minimal",
	}
	out, err := exec.Command(dotnetPath, args...).CombinedOutput()
	if err != nil {
		return fmt.Errorf("dotnet build: %w\n%s", err, strings.TrimSpace(string(out)))
	}
	return nil
}
