package types

import (
	"fmt"
	"io"
	"mochi/parser"
	"os"
)

// Env holds both type and value bindings for variables and functions.
type Env struct {
	parent *Env

	types  map[string]Type            // static types
	values map[string]any             // runtime values
	funcs  map[string]*parser.FunStmt // function declarations

	output io.Writer // default: os.Stdout
}

// NewEnv creates a new lexical scope environment.
func NewEnv(parent *Env) *Env {
	return &Env{
		parent: parent,
		types:  make(map[string]Type),
		values: make(map[string]any),
		funcs:  make(map[string]*parser.FunStmt),
		output: os.Stdout,
	}
}

// --- Type (Static) Binding ---

// SetVar defines a variable's static type.
func (e *Env) SetVar(name string, typ Type) {
	e.types[name] = typ
}

// GetVar looks up a variable's static type.
func (e *Env) GetVar(name string) (Type, error) {
	if t, ok := e.types[name]; ok {
		return t, nil
	}
	if e.parent != nil {
		return e.parent.GetVar(name)
	}
	return nil, fmt.Errorf("undefined variable: %s", name)
}

// --- Value (Runtime) Binding ---

// SetValue sets a variable's runtime value.
func (e *Env) SetValue(name string, val any) {
	e.values[name] = val
}

// UpdateValue modifies an existing variable's runtime value.
func (e *Env) UpdateValue(name string, val any) error {
	if _, ok := e.values[name]; ok {
		e.values[name] = val
		return nil
	}
	if e.parent != nil {
		return e.parent.UpdateValue(name, val)
	}
	return fmt.Errorf("variable not declared: %s", name)
}

// GetValue retrieves a runtime value.
func (e *Env) GetValue(name string) (any, error) {
	if val, ok := e.values[name]; ok {
		return val, nil
	}
	if e.parent != nil {
		return e.parent.GetValue(name)
	}
	return nil, fmt.Errorf("undefined variable: %s", name)
}

// --- Function Binding ---

// SetFunc binds a named function.
func (e *Env) SetFunc(name string, fn *parser.FunStmt) {
	e.funcs[name] = fn
}

// SetFuncType binds a function name to its static type.
func (e *Env) SetFuncType(name string, typ Type) {
	e.types[name] = typ
}

// GetFunc retrieves a function definition.
func (e *Env) GetFunc(name string) (*parser.FunStmt, bool) {
	if fn, ok := e.funcs[name]; ok {
		return fn, true
	}
	if e.parent != nil {
		return e.parent.GetFunc(name)
	}
	return nil, false
}

// Copy creates a shallow copy of the current environment with no parent.
// Useful for closures capturing current bindings.
func (e *Env) Copy() *Env {
	newEnv := &Env{
		parent: nil, // flatten parent chain
		types:  make(map[string]Type, len(e.types)),
		values: make(map[string]any, len(e.values)),
		funcs:  make(map[string]*parser.FunStmt, len(e.funcs)),
		output: e.output,
	}
	for k, v := range e.types {
		newEnv.types[k] = v
	}
	for k, v := range e.values {
		newEnv.values[k] = v
	}
	for k, v := range e.funcs {
		newEnv.funcs[k] = v
	}
	return newEnv
}

// --- Output Control ---

// SetWriter sets the output destination.
func (e *Env) SetWriter(w io.Writer) {
	e.output = w
}

// Writer returns the current output writer.
func (e *Env) Writer() io.Writer {
	return e.output
}
