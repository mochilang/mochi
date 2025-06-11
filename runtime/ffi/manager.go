package ffi

import (
	"fmt"
	"path/filepath"
	"strings"

	"mochi/parser"
	denoffi "mochi/runtime/ffi/deno"
	"mochi/runtime/ffi/extern"
	goffi "mochi/runtime/ffi/go"
	pythonffi "mochi/runtime/ffi/python"
)

// Manager tracks imported modules for different language runtimes and
// resolves values accessed through the interpreter.
type Manager struct {
	pyModules     map[string]string
	goModules     map[string]string
	tsModules     map[string]string
	externObjects map[string]*parser.ExternObjectDecl
}

// NewManager returns an empty FFI manager.
func NewManager() *Manager {
	return &Manager{
		pyModules:     map[string]string{},
		goModules:     map[string]string{},
		tsModules:     map[string]string{},
		externObjects: map[string]*parser.ExternObjectDecl{},
	}
}

// Import registers a module alias for the given language.
// repoRoot is used to resolve relative TypeScript paths.
func (m *Manager) Import(lang, alias, path, repoRoot string) error {
	mod := strings.Trim(path, "\"")
	switch lang {
	case "python":
		m.pyModules[alias] = mod
	case "go":
		m.goModules[alias] = mod
	case "typescript":
		if !strings.HasPrefix(mod, "http://") && !strings.HasPrefix(mod, "https://") && !strings.HasPrefix(mod, "file://") {
			mod = "file://" + filepath.Join(repoRoot, mod)
		}
		m.tsModules[alias] = mod
	default:
		return fmt.Errorf("unsupported import language: %s", lang)
	}
	return nil
}

// DeclareExternObject records an extern object declaration for later validation.
func (m *Manager) DeclareExternObject(decl *parser.ExternObjectDecl) {
	m.externObjects[decl.Name] = decl
}

// IsExternObjectDeclared reports if name was declared as an extern object.
func (m *Manager) IsExternObjectDeclared(name string) bool {
	_, ok := m.externObjects[name]
	return ok
}

// CheckExternObjects ensures all declared extern objects have been registered.
func (m *Manager) CheckExternObjects() error {
	for name, decl := range m.externObjects {
		if _, ok := extern.Get(name); !ok {
			return fmt.Errorf("%s: extern object not registered: %s", decl.Pos, name)
		}
	}
	return nil
}

// Lookup returns a placeholder value for the imported module if present.
func (m *Manager) Lookup(name string) (any, bool) {
	if mod, ok := m.pyModules[name]; ok {
		return PythonValue{Module: mod}, true
	}
	if mod, ok := m.goModules[name]; ok {
		return GoValue{Module: mod}, true
	}
	if mod, ok := m.tsModules[name]; ok {
		return TSValue{Module: mod}, true
	}
	return nil, false
}

// Append appends a field to the placeholder value if it is FFI related.
func (m *Manager) Append(val any, field string) (any, bool) {
	switch v := val.(type) {
	case PythonValue:
		v.Attrs = append(v.Attrs, field)
		return v, true
	case GoValue:
		v.Attrs = append(v.Attrs, field)
		return v, true
	case TSValue:
		v.Attrs = append(v.Attrs, field)
		return v, true
	}
	return val, false
}

// Final resolves the value if it refers to a foreign module attribute.
func (m *Manager) Final(val any) (any, error) {
	switch v := val.(type) {
	case PythonValue:
		return pythonffi.Attr(v.Module, strings.Join(v.Attrs, "."))
	case GoValue:
		name := v.Module
		if len(v.Attrs) > 0 {
			name += "." + strings.Join(v.Attrs, ".")
		}
		return goffi.Call(name)
	case TSValue:
		return denoffi.Attr(v.Module, strings.Join(v.Attrs, ":"))
	}
	return val, nil
}

// Call invokes a callable FFI value with arguments.
func (m *Manager) Call(val any, args []any) (any, error) {
	switch v := val.(type) {
	case PythonValue:
		return pythonffi.Attr(v.Module, strings.Join(v.Attrs, "."), args...)
	case GoValue:
		name := v.Module
		if len(v.Attrs) > 0 {
			name += "." + strings.Join(v.Attrs, ".")
		}
		return goffi.Call(name, args...)
	case TSValue:
		return denoffi.Attr(v.Module, strings.Join(v.Attrs, ":"), args...)
	}
	return nil, fmt.Errorf("value is not callable")
}

// IsValue reports whether the provided value is an FFI placeholder.
func (m *Manager) IsValue(val any) bool {
	switch val.(type) {
	case PythonValue, GoValue, TSValue:
		return true
	default:
		return false
	}
}
