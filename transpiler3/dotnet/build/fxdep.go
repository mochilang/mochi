package build

import (
	"fmt"
	"os/exec"
	"strings"
)

// packFxDependent publishes a framework-dependent .NET app to outDir.
// Produces ClassName.dll + ClassName.runtimeconfig.json in outDir.
// Requires the corresponding .NET runtime to be installed on the run host.
func packFxDependent(dotnetPath, projDir, outDir, tfm string) error {
	args := []string{
		"publish", projDir,
		"--self-contained", "false",
		"--framework", tfm,
		"--output", outDir,
		"--nologo",
		"-v", "minimal",
	}
	out, err := exec.Command(dotnetPath, args...).CombinedOutput()
	if err != nil {
		return fmt.Errorf("dotnet publish: %w\n%s", err, strings.TrimSpace(string(out)))
	}
	return nil
}
