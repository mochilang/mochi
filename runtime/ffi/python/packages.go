package python

import (
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
)

// PackageInfo describes an installed Python package.
type PackageInfo struct {
	Name     string `json:"name,omitempty"`
	Version  string `json:"version,omitempty"`
	Summary  string `json:"summary,omitempty"`
	Location string `json:"location,omitempty"`
}

// Packages returns information about all installed Python packages.
func Packages() ([]PackageInfo, error) {
	const pySrc = `import json, importlib.metadata, sys
pkgs = []
for dist in importlib.metadata.distributions():
    md = dist.metadata
    pkgs.append({
        "Name": md.get("Name", dist.metadata.get("Name", "")),
        "Version": dist.version,
        "Summary": md.get("Summary", ""),
        "Location": str(dist.locate_file("")),
    })
json.dump(pkgs, sys.stdout)`

	file, err := os.CreateTemp("", "mochi_py_pkgs_*.py")
	if err != nil {
		return nil, err
	}
	defer os.Remove(file.Name())
	if _, err := file.WriteString(pySrc); err != nil {
		file.Close()
		return nil, err
	}
	file.Close()

	cmd := exec.Command("python3", file.Name())
	out, err := cmd.CombinedOutput()
	if err != nil {
		return nil, fmt.Errorf("python error: %w\n%s", err, out)
	}

	var list []PackageInfo
	if err := json.Unmarshal(out, &list); err != nil {
		return nil, fmt.Errorf("decode error: %w\noutput: %s", err, out)
	}
	return list, nil
}
