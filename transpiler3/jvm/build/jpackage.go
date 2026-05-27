package build

import (
	"fmt"
	"os/exec"
	"path/filepath"
	"runtime"
)

// BuildJPackage produces an OS-native installer from outJar using jpackage.
// outDir is the directory where the installer is written.
// Returns ErrToolNotFound if jpackage is not available.
func BuildJPackage(tc *Toolchain, outJar, appName, outDir string) error {
	jpackagePath := filepath.Join(filepath.Dir(tc.Javac), "jpackage")
	if _, err := exec.LookPath(jpackagePath); err != nil {
		if _, err2 := exec.LookPath("jpackage"); err2 != nil {
			return fmt.Errorf("jpackage not found: %w", err)
		}
		jpackagePath = "jpackage"
	}

	pkgType := defaultPackageType()
	args := []string{
		"--input", filepath.Dir(outJar),
		"--main-jar", filepath.Base(outJar),
		"--name", appName,
		"--dest", outDir,
	}
	if pkgType != "" {
		args = append(args, "--type", pkgType)
	}

	cmd := exec.Command(jpackagePath, args...)
	out, err := cmd.CombinedOutput()
	if err != nil {
		return fmt.Errorf("jpackage: %w\n%s", err, out)
	}
	return nil
}

func defaultPackageType() string {
	switch runtime.GOOS {
	case "darwin":
		return "dmg"
	case "linux":
		return "deb"
	case "windows":
		return "msi"
	default:
		return ""
	}
}
