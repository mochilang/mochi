package tcc

import (
	"archive/tar"
	"bytes"
	"compress/bzip2"
	"fmt"
	"io"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
)

// EnsureTCC verifies that the TinyCC compiler is installed. It attempts a
// best-effort installation using common package managers on each platform.
func EnsureTCC() error {
	// If the TinyCC compiler and static library are already available,
	// nothing to do.
	if _, err := exec.LookPath("tcc"); err == nil {
		if _, err := findLib(); err == nil {
			return nil
		}
	}

	// Attempt installation via common package managers first.
	switch runtime.GOOS {
	case "darwin":
		if _, err := exec.LookPath("brew"); err == nil {
			cmd := exec.Command("brew", "install", "tinycc")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	case "linux":
		if _, err := exec.LookPath("apt-get"); err == nil {
			cmd := exec.Command("apt-get", "update")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				return err
			}
			cmd = exec.Command("apt-get", "install", "-y", "tcc", "libtcc-dev")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	case "windows":
		if _, err := exec.LookPath("choco"); err == nil {
			cmd := exec.Command("choco", "install", "-y", "tinycc")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		} else if _, err := exec.LookPath("scoop"); err == nil {
			cmd := exec.Command("scoop", "install", "tinycc")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	}

	if _, err := exec.LookPath("tcc"); err == nil {
		if _, err := findLib(); err == nil {
			return nil
		}
	}

	// Package managers failed or missing library: download sources and
	// build libtcc.a locally.
	fmt.Println("\U0001F528 Building TinyCC from source...")
	lib, _ := findLib()
	srcDir := filepath.Join("tools", "tcc", "tcc")
	if err := os.MkdirAll(srcDir, 0o755); err != nil {
		return err
	}
	if _, err := os.Stat(filepath.Join(srcDir, "configure")); err != nil {
		// Download and extract archive
		url := "https://download.savannah.gnu.org/releases/tinycc/tcc-0.9.27.tar.bz2"
		resp, err := http.Get(url)
		if err != nil {
			return fmt.Errorf("download tcc: %w", err)
		}
		defer resp.Body.Close()
		if resp.StatusCode != http.StatusOK {
			return fmt.Errorf("download tcc: %s", resp.Status)
		}
		data, err := io.ReadAll(resp.Body)
		if err != nil {
			return err
		}
		br := bzip2.NewReader(bytes.NewReader(data))
		tr := tar.NewReader(br)
		var prefix string
		for {
			hdr, err := tr.Next()
			if err == io.EOF {
				break
			}
			if err != nil {
				return err
			}
			name := hdr.Name
			if prefix == "" {
				if i := strings.IndexByte(name, '/'); i != -1 {
					prefix = name[:i+1]
					name = name[i+1:]
				}
			} else if strings.HasPrefix(name, prefix) {
				name = name[len(prefix):]
			}
			if name == "" {
				continue
			}
			target := filepath.Join(srcDir, name)
			if hdr.FileInfo().IsDir() {
				if err := os.MkdirAll(target, hdr.FileInfo().Mode()); err != nil {
					return err
				}
				continue
			}
			if err := os.MkdirAll(filepath.Dir(target), 0o755); err != nil {
				return err
			}
			out, err := os.OpenFile(target, os.O_CREATE|os.O_WRONLY|os.O_TRUNC, hdr.FileInfo().Mode())
			if err != nil {
				return err
			}
			if _, err := io.Copy(out, tr); err != nil {
				out.Close()
				return err
			}
			out.Close()
		}
	}

	// Configure and build libtcc.a
	cmd := exec.Command("./configure", "--enable-static")
	cmd.Dir = srcDir
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		return err
	}
	cmd = exec.Command("make", "libtcc.a")
	cmd.Dir = srcDir
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		return err
	}
	if _, err := os.Stat(lib); err != nil {
		return fmt.Errorf("libtcc.a not found after build")
	}
	return nil
}

func findLib() (string, error) {
	path := os.Getenv("TCC_LIB")
	if path == "" {
		path = filepath.Join("tools", "tcc", "tcc", "libtcc.a")
	}
	if _, err := os.Stat(path); err != nil {
		return path, err
	}
	return path, nil
}
