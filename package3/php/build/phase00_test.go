package build

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestPhase0Skeleton is the per-phase sentinel test. The CI gate for phase
// 0 LANDED requires this test to pass on every supported host.
//
// Sub-tests:
//   - end_to_end: Driver allocates work-dir + cache-dir, PrepareWorkspace
//     creates all expected sub-directories, Cleanup removes the work-dir.
//   - package_layout: the package3/php/ tree exists at the expected
//     paths and has all documented Go packages.
//   - workspace_subdirs: EnsureSubDirs creates shims/, glue/, vendor/
//     under the workspace root.
func TestPhase0Skeleton(t *testing.T) {
	t.Run("end_to_end", func(t *testing.T) {
		d := NewDriver(Options{NoCache: true})
		defer d.Cleanup() //nolint:errcheck

		if d.WorkDir() != "" {
			t.Errorf("WorkDir should be empty before PrepareWorkspace; got %q", d.WorkDir())
		}

		w, err := d.PrepareWorkspace()
		if err != nil {
			t.Fatalf("PrepareWorkspace: %v", err)
		}
		if w.Root == "" {
			t.Fatalf("Workspace.Root is empty after PrepareWorkspace")
		}
		if _, err := os.Stat(w.Root); err != nil {
			t.Fatalf("Workspace.Root %q does not exist: %v", w.Root, err)
		}
		if !strings.Contains(w.Root, "mochi-php-") {
			t.Errorf("WorkDir %q should contain mochi-php- prefix", w.Root)
		}

		workDir := w.Root
		if err := d.Cleanup(); err != nil {
			t.Fatalf("Cleanup: %v", err)
		}
		if _, err := os.Stat(workDir); !os.IsNotExist(err) {
			t.Errorf("work-dir %q still exists after Cleanup", workDir)
		}
		if d.WorkDir() != "" {
			t.Errorf("WorkDir should be empty after Cleanup; got %q", d.WorkDir())
		}
	})

	t.Run("package_layout", func(t *testing.T) {
		// Locate package3/php/ relative to this test source file. The
		// test runs from package3/php/build/, so the package root is
		// one directory up.
		here, err := os.Getwd()
		if err != nil {
			t.Fatalf("Getwd: %v", err)
		}
		pkgRoot := filepath.Dir(here)
		expected := []string{
			"README.md",
			"build/driver.go",
			"errors/errors.go",
			"packagist/packagist.go",
			"cache/cache.go",
			"reflect/reflect.go",
			"typemap/typemap.go",
			"externemit/externemit.go",
			"glue/glue.go",
			"autoload/autoload.go",
			"lock/lock.go",
			"library/library.go",
			"publish/publish.go",
		}
		for _, rel := range expected {
			path := filepath.Join(pkgRoot, rel)
			info, err := os.Stat(path)
			if err != nil {
				t.Errorf("expected file %s missing: %v", rel, err)
				continue
			}
			if info.IsDir() {
				t.Errorf("expected file %s is a directory", rel)
			}
		}
	})

	t.Run("workspace_subdirs", func(t *testing.T) {
		d := NewDriver(Options{NoCache: true})
		defer d.Cleanup() //nolint:errcheck

		w, err := d.PrepareWorkspace()
		if err != nil {
			t.Fatalf("PrepareWorkspace: %v", err)
		}
		if err := w.EnsureSubDirs(); err != nil {
			t.Fatalf("EnsureSubDirs: %v", err)
		}
		for name, dir := range map[string]string{
			"ShimDir":   w.ShimDir,
			"GlueDir":   w.GlueDir,
			"VendorDir": w.VendorDir,
		} {
			if dir == "" {
				t.Errorf("%s is empty after EnsureSubDirs", name)
				continue
			}
			if _, err := os.Stat(dir); err != nil {
				t.Errorf("%s %q does not exist: %v", name, dir, err)
			}
		}
		if !strings.HasSuffix(w.ShimDir, "shims") {
			t.Errorf("ShimDir %q should end with shims", w.ShimDir)
		}
		if !strings.HasSuffix(w.GlueDir, "glue") {
			t.Errorf("GlueDir %q should end with glue", w.GlueDir)
		}
		if !strings.HasSuffix(w.VendorDir, "vendor") {
			t.Errorf("VendorDir %q should end with vendor", w.VendorDir)
		}
	})
}
