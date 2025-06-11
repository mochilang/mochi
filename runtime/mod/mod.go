package mod

import (
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"sync"

	"golang.org/x/mod/modfile"
)

// Module represents a Mochi module identified by a go.mod file.
type Module struct {
	// Root is the directory containing go.mod.
	Root string
	// Path is the module path declared in go.mod.
	Path string
}

var cache struct {
	sync.Mutex
	modules map[string]*Module
}

func init() {
	cache.modules = map[string]*Module{}
}

// FindRoot walks parent directories starting at dir until a go.mod file is found.
func FindRoot(dir string) (string, error) {
	d := dir
	for {
		if _, err := os.Stat(filepath.Join(d, "go.mod")); err == nil {
			return d, nil
		}
		parent := filepath.Dir(d)
		if parent == d {
			break
		}
		d = parent
	}
	return "", fmt.Errorf("module root not found from %s", dir)
}

// Load discovers the module starting from dir. The module is cached for
// subsequent calls.
func Load(dir string) (*Module, error) {
	root, err := FindRoot(dir)
	if err != nil {
		return nil, err
	}

	cache.Lock()
	if m, ok := cache.modules[root]; ok {
		cache.Unlock()
		return m, nil
	}
	cache.Unlock()

	data, err := os.ReadFile(filepath.Join(root, "go.mod"))
	if err != nil {
		return nil, err
	}
	mf, err := modfile.Parse("go.mod", data, nil)
	if err != nil {
		return nil, err
	}
	mod := &Module{Root: root}
	if mf.Module != nil {
		mod.Path = mf.Module.Mod.Path
	}

	cache.Lock()
	cache.modules[root] = mod
	cache.Unlock()
	return mod, nil
}

// Resolve returns an absolute path for a module-relative import.
func (m *Module) Resolve(p string) string {
	return filepath.Join(m.Root, filepath.FromSlash(p))
}

// Exists reports whether a given module-relative path exists.
func (m *Module) Exists(p string) bool {
	_, err := os.Stat(m.Resolve(p))
	return err == nil
}

// ReadFile reads a file relative to the module root.
func (m *Module) ReadFile(p string) ([]byte, error) {
	return os.ReadFile(m.Resolve(p))
}

// Open opens a file relative to the module root.
func (m *Module) Open(p string) (fs.File, error) {
	return os.Open(m.Resolve(p))
}
