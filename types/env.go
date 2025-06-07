package types

import (
	"fmt"
	"io"
	"mochi/parser"
	"os"
)

// ModelSpec defines a named model configuration.
type ModelSpec struct {
	Provider string
	Name     string
	Params   map[string]any
}

// Env holds both type and value bindings for variables and functions.
type Env struct {
	parent *Env

	types   map[string]Type            // static types
	structs map[string]StructType      // user-defined struct types
	unions  map[string]UnionType       // user-defined union types
	streams map[string]StructType      // stream declarations
	mut     map[string]bool            // mutability of variables
	values  map[string]any             // runtime values
	funcs   map[string]*parser.FunStmt // function declarations
	models  map[string]ModelSpec       // model aliases

	output io.Writer // default: os.Stdout
}

// NewEnv creates a new lexical scope environment.
func NewEnv(parent *Env) *Env {
	var out io.Writer = os.Stdout
	if parent != nil {
		out = parent.output
	}
	return &Env{
		parent:  parent,
		types:   make(map[string]Type),
		structs: make(map[string]StructType),
		unions:  make(map[string]UnionType),
		streams: make(map[string]StructType),
		mut:     make(map[string]bool),
		values:  make(map[string]any),
		funcs:   make(map[string]*parser.FunStmt),
		models:  make(map[string]ModelSpec),
		output:  out,
	}
}

// --- Type (Static) Binding ---

// SetStruct defines a user-defined struct type.
func (e *Env) SetStruct(name string, st StructType) {
	e.structs[name] = st
}

// GetStruct retrieves a struct type by name.
func (e *Env) GetStruct(name string) (StructType, bool) {
	if t, ok := e.structs[name]; ok {
		return t, true
	}
	if e.parent != nil {
		return e.parent.GetStruct(name)
	}
	return StructType{}, false
}

// SetUnion defines a user-defined union type.
func (e *Env) SetUnion(name string, ut UnionType) { e.unions[name] = ut }

// GetUnion retrieves a union type by name.
func (e *Env) GetUnion(name string) (UnionType, bool) {
	if u, ok := e.unions[name]; ok {
		return u, true
	}
	if e.parent != nil {
		return e.parent.GetUnion(name)
	}
	return UnionType{}, false
}

// SetStream defines a stream declaration.
func (e *Env) SetStream(name string, st StructType) { e.streams[name] = st }

// GetStream retrieves a stream by name.
func (e *Env) GetStream(name string) (StructType, bool) {
	if st, ok := e.streams[name]; ok {
		return st, true
	}
	if e.parent != nil {
		return e.parent.GetStream(name)
	}
	return StructType{}, false
}

// FindUnionByVariant returns the union type that contains the given variant name.
func (e *Env) FindUnionByVariant(variant string) (UnionType, bool) {
	for _, u := range e.unions {
		if _, ok := u.Variants[variant]; ok {
			return u, true
		}
	}
	if e.parent != nil {
		return e.parent.FindUnionByVariant(variant)
	}
	return UnionType{}, false
}

// SetModel defines a model alias.
func (e *Env) SetModel(name string, spec ModelSpec) { e.models[name] = spec }

// GetModel retrieves a model alias.
func (e *Env) GetModel(name string) (ModelSpec, bool) {
	if m, ok := e.models[name]; ok {
		return m, true
	}
	if e.parent != nil {
		return e.parent.GetModel(name)
	}
	return ModelSpec{}, false
}

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
		parent:  nil, // flatten parent chain
		types:   make(map[string]Type, len(e.types)),
		mut:     make(map[string]bool, len(e.mut)),
		values:  make(map[string]any, len(e.values)),
		funcs:   make(map[string]*parser.FunStmt, len(e.funcs)),
		structs: make(map[string]StructType, len(e.structs)),
		unions:  make(map[string]UnionType, len(e.unions)),
		streams: make(map[string]StructType, len(e.streams)),
		models:  make(map[string]ModelSpec, len(e.models)),
		output:  e.output,
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
	for k, v := range e.structs {
		newEnv.structs[k] = v
	}
	for k, v := range e.unions {
		newEnv.unions[k] = v
	}
	for k, v := range e.streams {
		newEnv.streams[k] = v
	}
	for k, v := range e.models {
		newEnv.models[k] = v
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
