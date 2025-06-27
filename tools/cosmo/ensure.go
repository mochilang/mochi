package cosmo

import (
	"archive/zip"
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"os"
	"path/filepath"
	"strings"
)

// EnsureCosmo verifies that the static Cosmopolitan library is installed.
// The library is expected at $COSMO_LIB or tools/cosmo/cosmo/libcosmo.a.

func EnsureCosmo() error {
	path := os.Getenv("COSMO_LIB")
	if path == "" {
		path = filepath.Join("tools", "cosmo", "cosmo", "libcosmo.a")
	}
	if _, err := os.Stat(path); err == nil {
		return nil
	}

	fmt.Println("\U0001F680 Installing Cosmopolitan...")
	if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
		return err
	}

	rel, err := latestRelease()
	if err != nil {
		return err
	}
	url := fmt.Sprintf("https://github.com/jart/cosmopolitan/releases/download/%s/cosmocc-%s.zip", rel.TagName, rel.TagName)

	resp, err := http.Get(url)
	if err != nil {
		return fmt.Errorf("download cosmo: %w", err)
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		return fmt.Errorf("download cosmo: %s", resp.Status)
	}
	data, err := io.ReadAll(resp.Body)
	if err != nil {
		return err
	}
	zr, err := zip.NewReader(bytes.NewReader(data), int64(len(data)))
	if err != nil {
		return err
	}
	for _, f := range zr.File {
		if strings.HasSuffix(f.Name, "libcosmo.a") {
			rc, err := f.Open()
			if err != nil {
				return err
			}
			defer rc.Close()
			out, err := os.Create(path)
			if err != nil {
				return err
			}
			defer out.Close()
			if _, err := io.Copy(out, rc); err != nil {
				return err
			}
			return nil
		}
	}
	return fmt.Errorf("libcosmo.a not found in archive")
}

type release struct {
	TagName string `json:"tag_name"`
}

func latestRelease() (*release, error) {
	resp, err := http.Get("https://api.github.com/repos/jart/cosmopolitan/releases/latest")
	if err != nil {
		return nil, fmt.Errorf("fetch release: %w", err)
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("fetch release: %s", resp.Status)
	}
	var r release
	if err := json.NewDecoder(resp.Body).Decode(&r); err != nil {
		return nil, err
	}
	if r.TagName == "" {
		return nil, fmt.Errorf("invalid release response")
	}
	return &r, nil
}
