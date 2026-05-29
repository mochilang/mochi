package build

import (
	"os"
	"path/filepath"
)

// goModContent is the go.mod template emitted into each work
// directory. The module name is per-build (defaults to
// "mochi_userprog"); the Go floor is 1.26.0; toolchain pinned
// to 1.26.3.
func goModContent(modulePath string) string {
	if modulePath == "" {
		modulePath = "mochi_userprog"
	}
	return "module " + modulePath + "\n\ngo 1.26.0\n\ntoolchain go1.26.3\n"
}

// writeGoMod writes go.mod into dir with the given module path.
func writeGoMod(dir, modulePath string) error {
	return os.WriteFile(filepath.Join(dir, "go.mod"), []byte(goModContent(modulePath)), 0o644)
}
