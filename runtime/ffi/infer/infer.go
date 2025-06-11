package ffiinfo

// ModuleInfo describes a module's exported symbols and documentation.
type ModuleInfo struct {
	Path     string        `json:"path,omitempty"`
	Doc      string        `json:"doc,omitempty"`
	Examples []ExampleInfo `json:"examples,omitempty"`

	Functions []FuncInfo  `json:"functions,omitempty"`
	Vars      []VarInfo   `json:"vars,omitempty"`
	Consts    []ConstInfo `json:"consts,omitempty"`
	Types     []TypeInfo  `json:"types,omitempty"`
}

// FuncInfo describes an exported function.
type FuncInfo struct {
	Name     string        `json:"name,omitempty"`
	Doc      string        `json:"doc,omitempty"`
	Params   []ParamInfo   `json:"params,omitempty"`
	Results  []ParamInfo   `json:"results,omitempty"`
	Examples []ExampleInfo `json:"examples,omitempty"`
}

// VarInfo describes an exported variable.
type VarInfo struct {
	Name string `json:"name,omitempty"`
	Type string `json:"type,omitempty"`
	Doc  string `json:"doc,omitempty"`
}

// ConstInfo describes an exported constant.
type ConstInfo struct {
	Name  string `json:"name,omitempty"`
	Type  string `json:"type,omitempty"`
	Value string `json:"value,omitempty"`
	Doc   string `json:"doc,omitempty"`
}

// TypeInfo describes an exported type.
type TypeInfo struct {
	Name     string        `json:"name,omitempty"`
	Kind     string        `json:"kind,omitempty"`
	Doc      string        `json:"doc,omitempty"`
	Fields   []FieldInfo   `json:"fields,omitempty"`
	Methods  []FuncInfo    `json:"methods,omitempty"`
	Examples []ExampleInfo `json:"examples,omitempty"`
}

type FieldInfo struct {
	Name string `json:"name,omitempty"`
	Type string `json:"type,omitempty"`
	Tag  string `json:"tag,omitempty"`
}

type ParamInfo struct {
	Name string `json:"name,omitempty"`
	Type string `json:"type,omitempty"`
}

type ExampleInfo struct {
	Name   string `json:"name,omitempty"`
	Suffix string `json:"suffix,omitempty"`
	Doc    string `json:"doc,omitempty"`
	Code   string `json:"code,omitempty"`
	Output string `json:"output,omitempty"`
}
