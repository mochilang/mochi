package emit

import (
	"fmt"
	"os/exec"
	"strings"
)

// Compile invokes `dotnet build` on the project at projDir to compile into outDir.
// tfm is the target framework moniker, e.g. "net10.0".
// dotnetPath is the absolute path to the dotnet binary.
func Compile(projDir, outDir, tfm, dotnetPath string) error {
	args := []string{
		"build", projDir,
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
