package build

import (
	"fmt"
	"os"
	"path/filepath"
)

// writePackageLayout writes the canonical Phase 1 src/<pkg>/ tree:
//
//	<workDir>/
//	  pyproject.toml
//	  src/<pkg>/
//	    __init__.py
//	    __main__.py
//	    py.typed
//	    generated/
//	      __init__.py
//	      <module>.py  (already written by emit)
//
// The generated/<module>.py file must already exist when this is called.
func writePackageLayout(workDir, pkgName, moduleName string) error {
	pkgDir := filepath.Join(workDir, "src", pkgName)
	generatedDir := filepath.Join(pkgDir, "generated")

	files := map[string]string{
		filepath.Join(pkgDir, "__init__.py"): initPy(moduleName),
		filepath.Join(pkgDir, "__main__.py"): mainPy(moduleName),
		filepath.Join(pkgDir, "py.typed"):    "",
		filepath.Join(generatedDir, "__init__.py"): "",
		filepath.Join(workDir, "pyproject.toml"):   pyprojectToml(pkgName),
	}
	for path, content := range files {
		if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
			return err
		}
		if err := os.WriteFile(path, []byte(content), 0o644); err != nil {
			return fmt.Errorf("python build: write %s: %w", path, err)
		}
	}
	return nil
}

func initPy(moduleName string) string {
	return "from __future__ import annotations\n" +
		"\n" +
		"from .generated." + moduleName + " import main as main\n" +
		"\n" +
		"__all__ = [\"main\"]\n"
}

func mainPy(moduleName string) string {
	return "from __future__ import annotations\n" +
		"\n" +
		"from .generated." + moduleName + " import main\n" +
		"\n" +
		"if __name__ == \"__main__\":\n" +
		"    main()\n"
}

// pyprojectToml renders a minimal PEP 621 pyproject with the hatchling backend.
// Phase 15 will extend this with the wheel build target and project.scripts.
func pyprojectToml(pkgName string) string {
	return fmt.Sprintf(`[build-system]
requires = ["hatchling>=1.25"]
build-backend = "hatchling.build"

[project]
name = "%s"
version = "0.1.0"
requires-python = ">=3.12"
dependencies = [
    "mochi-runtime>=0.1.0",
]

[tool.hatch.build.targets.wheel]
packages = ["src/%s"]
`, pkgName, pkgName)
}
