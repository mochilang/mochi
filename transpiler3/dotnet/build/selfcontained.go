package build

import (
	"fmt"
	"os/exec"
	"strings"
)

// packSelfContained publishes a self-contained .NET application to outDir.
// Produces a self-contained publish directory for the host RID.
func packSelfContained(dotnetPath, projDir, outDir, tfm string) error {
	rid := hostRID()
	args := []string{
		"publish", projDir,
		"--self-contained", "true",
		"-r", rid,
		"--framework", tfm,
		"--output", outDir,
		"--nologo",
		"-v", "minimal",
	}
	out, err := exec.Command(dotnetPath, args...).CombinedOutput()
	if err != nil {
		return fmt.Errorf("dotnet publish (self-contained): %w\n%s", err, strings.TrimSpace(string(out)))
	}
	return nil
}
