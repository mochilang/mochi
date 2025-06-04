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
	mut    map[string]bool            // mutability of variables
	values map[string]any             // runtime values
	funcs  map[string]*parser.FunStmt // function declarations

	output io.Writer // default: os.Stdout
}

// NewEnv creates a new lexical scope environment.
func NewEnv(parent *Env) *Env {
	var out io.Writer = os.Stdout
	if parent != nil {
		out = parent.output
	}
	return &Env{
		parent: parent,
		types:  make(map[string]Type),
		mut:    make(map[string]bool),
		values: make(map[string]any),
		funcs:  make(map[string]*parser.FunStmt),
		output: out,
	}
}

// --- Type (Static) Binding ---

// SetVar defines a variable's static type.
func (e *Env) SetVar(name string, typ Type, mutable bool) {
	e.types[name] = typ
	e.mut[name] = mutable
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

func (e *Env) isMutable(name string) (bool, bool) {
	if m, ok := e.mut[name]; ok {
		return m, true
	}
	if e.parent != nil {
		return e.parent.isMutable(name)
	}
	return false, false
}

func (e *Env) IsMutable(name string) (bool, error) {
	if m, ok := e.isMutable(name); ok {
		return m, nil
	}
	return false, fmt.Errorf("undefined variable: %s", name)
}

// --- Value (Runtime) Binding ---

// SetValue sets a variable's runtime value.
func (e *Env) SetValue(name string, val any, mutable bool) {
	e.values[name] = val
	e.mut[name] = mutable
}

// UpdateValue modifies an existing variable's runtime value.
func (e *Env) UpdateValue(name string, val any) error {
	if _, ok := e.values[name]; ok {
		mutable, _ := e.isMutable(name)
		if !mutable {
			return fmt.Errorf("cannot assign to immutable variable: %s", name)
		}
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
		mut:    make(map[string]bool, len(e.mut)),
		values: make(map[string]any, len(e.values)),
		funcs:  make(map[string]*parser.FunStmt, len(e.funcs)),
		output: e.output,
	}
	for k, v := range e.types {
		newEnv.types[k] = v
	}
	for k, v := range e.mut {
		newEnv.mut[k] = v
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
