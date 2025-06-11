package ffiinfo

// ModuleInfo describes a module's exported symbols and documentation.
type ModuleInfo struct {
	Path     string
	Doc      string
	Examples []ExampleInfo

	Functions []FuncInfo
	Vars      []VarInfo
	Consts    []ConstInfo
	Types     []TypeInfo
}

// FuncInfo describes an exported function.
type FuncInfo struct {
	Name     string
	Doc      string
	Params   []ParamInfo
	Results  []ParamInfo
	Examples []ExampleInfo
}

// VarInfo describes an exported variable.
type VarInfo struct {
	Name string
	Type string
	Doc  string
}

// ConstInfo describes an exported constant.
type ConstInfo struct {
	Name  string
	Type  string
	Value string
	Doc   string
}

// TypeInfo describes an exported type.
type TypeInfo struct {
	Name     string
	Kind     string
	Doc      string
	Fields   []FieldInfo
	Methods  []FuncInfo
	Examples []ExampleInfo
}

type FieldInfo struct {
	Name string
	Type string
	Tag  string
}

type ParamInfo struct {
	Name string
	Type string
}

type ExampleInfo struct {
	Name   string
	Suffix string
	Doc    string
	Code   string
	Output string
}
