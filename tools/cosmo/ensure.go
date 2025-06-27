package cosmo

import (
	"archive/zip"
	"fmt"
	"io"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
)

const cosmoVersion = "4.0.2"
const cosmoURL = "https://justine.lol/cosmopolitan/cosmocc-" + cosmoVersion + ".zip"

// EnsureCosmo verifies that the cosmocc compiler is installed. If it isn't
// present in PATH, the prebuilt archive is downloaded and the binary extracted.
func EnsureCosmo() error {
	if _, err := exec.LookPath("cosmocc"); err == nil {
		return nil
	}
	fmt.Printf("\U0001F534 Downloading cosmocc %s...\n", cosmoVersion)
	resp, err := http.Get(cosmoURL)
	if err != nil {
		return err
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		return fmt.Errorf("download failed: %s", resp.Status)
	}
	tmpZip, err := os.CreateTemp("", "cosmocc-*.zip")
	if err != nil {
		return err
	}
	if _, err := io.Copy(tmpZip, resp.Body); err != nil {
		tmpZip.Close()
		return err
	}
	tmpZip.Close()
	defer os.Remove(tmpZip.Name())

	r, err := zip.OpenReader(tmpZip.Name())
	if err != nil {
		return err
	}
	defer r.Close()

	binName := "cosmocc"
	if runtime.GOOS == "windows" {
		binName += ".exe"
	}
	var extracted string
	for _, f := range r.File {
		if filepath.Base(f.Name) == binName {
			rc, err := f.Open()
			if err != nil {
				return err
			}
			tmpBin, err := os.CreateTemp("", binName)
			if err != nil {
				rc.Close()
				return err
			}
			if _, err := io.Copy(tmpBin, rc); err != nil {
				rc.Close()
				tmpBin.Close()
				return err
			}
			rc.Close()
			tmpBin.Close()
			if err := os.Chmod(tmpBin.Name(), 0755); err != nil {
				return err
			}
			extracted = tmpBin.Name()
			break
		}
	}
	if extracted == "" {
		return fmt.Errorf("%s not found in archive", binName)
	}

	target := filepath.Join("/usr/local/bin", binName)
	if err := exec.Command("install", "-m", "755", extracted, target).Run(); err == nil {
		return nil
	}

	home, err := os.UserHomeDir()
	if err != nil {
		home = "."
	}
	dest := filepath.Join(home, "bin", binName)
	if err := os.MkdirAll(filepath.Dir(dest), 0755); err != nil {
		return err
	}
	if err := os.Rename(extracted, dest); err != nil {
		return err
	}
	return nil
}
