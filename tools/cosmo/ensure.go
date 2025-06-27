package cosmo

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
)

// EnsureCosmo verifies that the cosmocc compiler is installed. It attempts a
// best-effort download using curl or wget if possible.
func EnsureCosmo() error {
	if _, err := exec.LookPath("cosmocc"); err == nil {
		return nil
	}
	url := "https://justine.lol/cosmopolitan/cosmocc"
	binDir := filepath.Join("bin")
	if err := os.MkdirAll(binDir, 0o755); err != nil {
		return err
	}
	dst := filepath.Join(binDir, "cosmocc")
	var cmd *exec.Cmd
	if _, err := exec.LookPath("curl"); err == nil {
		cmd = exec.Command("curl", "-fLo", dst, url)
	} else if _, err := exec.LookPath("wget"); err == nil {
		cmd = exec.Command("wget", "-O", dst, url)
	}
	if cmd != nil {
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		if err := cmd.Run(); err == nil {
			_ = os.Chmod(dst, 0o755)
		}
	}
	if p, err := exec.LookPath("cosmocc"); err == nil {
		if runtime.GOOS != "windows" {
			return os.Symlink(p, filepath.Join(binDir, "cosmocc"))
		}
		return nil
	}
	return fmt.Errorf("cosmocc not found")
}
