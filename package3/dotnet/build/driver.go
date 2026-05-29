// Package build drives the end-to-end MEP-68 bridge build pipeline.
// It wires together metacli surface resolution, shimgen synthesis,
// emit, clrhosting bridge codegen, and workspace materialisation.
package build

import (
	"fmt"
	"os"
	"path/filepath"

	"mochi/package3/dotnet/publish"
	"mochi/package3/dotnet/shimgen"
)

// Driver holds the filesystem and toolchain configuration for the build pipeline.
type Driver struct {
	// WorkDir is the root directory for the generated workspace.
	WorkDir string
}

// NewDriver creates a Driver with the given work directory.
func NewDriver(workDir string) *Driver {
	return &Driver{WorkDir: workDir}
}

// PrepareWorkspace ensures the work directory exists.
func (d *Driver) PrepareWorkspace() error {
	if d.WorkDir == "" {
		dir, err := os.MkdirTemp("", "mochi-dotnet-")
		if err != nil {
			return fmt.Errorf("driver: allocate work-dir: %w", err)
		}
		d.WorkDir = dir
		return nil
	}
	if err := os.MkdirAll(d.WorkDir, 0o755); err != nil {
		return fmt.Errorf("driver: create work-dir %s: %w", d.WorkDir, err)
	}
	return nil
}

// WriteWorkspaceRoot writes the top-level .sln and returns its path.
func (d *Driver) WriteWorkspaceRoot(cfg publish.WorkspaceConfig, shims []*shimgen.Shim) (string, error) {
	if d.WorkDir == "" {
		return "", fmt.Errorf("driver: WriteWorkspaceRoot called before PrepareWorkspace")
	}
	cfg.WorkDir = filepath.Join(d.WorkDir, "dotnet_workspace")
	return publish.MaterialiseWorkspace(cfg, shims)
}
