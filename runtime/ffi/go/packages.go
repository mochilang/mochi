//go:build !tinygo

package goffi

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"os/exec"
	"strings"
)

// PackageInfo describes a Go package available to the runtime.
type PackageInfo struct {
	Path string // Import path
	Doc  string // Package documentation summary
}

// Packages returns information about all Go packages accessible in the current environment.
// It invokes `go list -json all` and parses the output into a slice of PackageInfo.
func Packages() ([]PackageInfo, error) {
	cmd := exec.Command("go", "list", "-e", "-json", "all")
	out, err := cmd.Output()
	if err != nil {
		return nil, fmt.Errorf("go list failed: %w", err)
	}

	dec := json.NewDecoder(bytes.NewReader(out))
	infos := []PackageInfo{}
	for {
		var pkg struct {
			ImportPath string
			Doc        string
		}
		if err := dec.Decode(&pkg); err != nil {
			if err == io.EOF {
				break
			}
			return nil, fmt.Errorf("decode error: %w", err)
		}
		infos = append(infos, PackageInfo{Path: pkg.ImportPath, Doc: strings.TrimSpace(pkg.Doc)})
	}
	return infos, nil
}
