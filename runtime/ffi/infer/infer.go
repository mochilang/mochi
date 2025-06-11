package ffiinfo

// ModuleInfo describes the exported symbols of a foreign module.
type ModuleInfo struct {
	Path      string
	Functions []FuncInfo
	Vars      []VarInfo
	Consts    []ConstInfo
	Types     []TypeInfo
}

// FuncInfo describes an exported function.
type FuncInfo struct {
	Name      string
	Signature string
}

// VarInfo describes an exported variable.
type VarInfo struct {
	Name string
	Type string
}

// ConstInfo describes an exported constant.
type ConstInfo struct {
	Name  string
	Type  string
	Value string
}

// TypeInfo describes an exported type.
type TypeInfo struct {
	Name string
	Kind string
}
