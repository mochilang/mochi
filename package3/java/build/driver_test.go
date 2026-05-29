package build

import (
	"os"
	"path/filepath"
	"testing"
)

func TestNewDriver(t *testing.T) {
	d := NewDriver(Options{NoCache: true})
	if d == nil {
		t.Fatal("NewDriver returned nil")
	}
	if d.CacheDir() != "" {
		t.Errorf("CacheDir() = %q; want empty (NoCache=true)", d.CacheDir())
	}
	if d.WorkDir() != "" {
		t.Errorf("WorkDir() = %q; want empty before PrepareWorkspace", d.WorkDir())
	}
}

func TestDriverDefaultCacheDir(t *testing.T) {
	d := NewDriver(Options{})
	if d.CacheDir() == "" {
		t.Error("CacheDir() is empty without NoCache")
	}
}

func TestDriverPrepareWorkspace(t *testing.T) {
	d := NewDriver(Options{NoCache: true})
	w, err := d.PrepareWorkspace()
	if err != nil {
		t.Fatalf("PrepareWorkspace: %v", err)
	}
	defer d.Cleanup()
	if w == nil {
		t.Fatal("PrepareWorkspace returned nil workspace")
	}
	if d.WorkDir() == "" {
		t.Error("WorkDir() is empty after PrepareWorkspace")
	}
	if _, err := os.Stat(d.WorkDir()); err != nil {
		t.Errorf("work-dir does not exist: %v", err)
	}
}

func TestDriverCleanup(t *testing.T) {
	d := NewDriver(Options{NoCache: true})
	_, err := d.PrepareWorkspace()
	if err != nil {
		t.Fatalf("PrepareWorkspace: %v", err)
	}
	workDir := d.WorkDir()
	if err := d.Cleanup(); err != nil {
		t.Fatalf("Cleanup: %v", err)
	}
	if _, err := os.Stat(workDir); !os.IsNotExist(err) {
		t.Errorf("work-dir still exists after Cleanup: %v", err)
	}
}

func TestDriverWriteWorkspaceRoot(t *testing.T) {
	d := NewDriver(Options{NoCache: true})
	w, err := d.PrepareWorkspace()
	if err != nil {
		t.Fatalf("PrepareWorkspace: %v", err)
	}
	defer d.Cleanup()

	root, err := d.WriteWorkspaceRoot(w)
	if err != nil {
		t.Fatalf("WriteWorkspaceRoot: %v", err)
	}
	pomPath := filepath.Join(root, "pom.xml")
	if _, err := os.Stat(pomPath); err != nil {
		t.Errorf("pom.xml not found at %s: %v", pomPath, err)
	}
}

func TestDriverWriteWorkspaceRootBeforePrepare(t *testing.T) {
	d := NewDriver(Options{NoCache: true})
	_, err := d.WriteWorkspaceRoot(DefaultWorkspace())
	if err == nil {
		t.Error("expected error when calling WriteWorkspaceRoot before PrepareWorkspace")
	}
}
