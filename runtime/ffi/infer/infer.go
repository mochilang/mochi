package ffiinfo

// ModuleInfo describes a module's exported symbols and documentation.
type ModuleInfo struct {
	Path     string        `json:"Path,omitempty"`
	Doc      string        `json:"Doc,omitempty"`
	Examples []ExampleInfo `json:"Examples,omitempty"`

	Functions []FuncInfo  `json:"Functions,omitempty"`
	Vars      []VarInfo   `json:"Vars,omitempty"`
	Consts    []ConstInfo `json:"Consts,omitempty"`
	Types     []TypeInfo  `json:"Types,omitempty"`
}

// FuncInfo describes an exported function.
type FuncInfo struct {
	Name     string        `json:"Name,omitempty"`
	Doc      string        `json:"Doc,omitempty"`
	Params   []ParamInfo   `json:"Params,omitempty"`
	Results  []ParamInfo   `json:"Results,omitempty"`
	Examples []ExampleInfo `json:"Examples,omitempty"`
}

// VarInfo describes an exported variable.
type VarInfo struct {
	Name string `json:"Name,omitempty"`
	Type string `json:"Type,omitempty"`
	Doc  string `json:"Doc,omitempty"`
}

// ConstInfo describes an exported constant.
type ConstInfo struct {
	Name  string `json:"Name,omitempty"`
	Type  string `json:"Type,omitempty"`
	Value string `json:"Value,omitempty"`
	Doc   string `json:"Doc,omitempty"`
}

// TypeInfo describes an exported type.
type TypeInfo struct {
	Name     string        `json:"Name,omitempty"`
	Kind     string        `json:"Kind,omitempty"`
	Doc      string        `json:"Doc,omitempty"`
	Fields   []FieldInfo   `json:"Fields,omitempty"`
	Methods  []FuncInfo    `json:"Methods,omitempty"`
	Examples []ExampleInfo `json:"Examples,omitempty"`
}

type FieldInfo struct {
	Name string `json:"Name,omitempty"`
	Type string `json:"Type,omitempty"`
	Tag  string `json:"Tag,omitempty"`
}

type ParamInfo struct {
	Name string `json:"Name,omitempty"`
	Type string `json:"Type,omitempty"`
}

type ExampleInfo struct {
	Name   string `json:"Name,omitempty"`
	Suffix string `json:"Suffix,omitempty"`
	Doc    string `json:"Doc,omitempty"`
	Code   string `json:"Code,omitempty"`
	Output string `json:"Output,omitempty"`
}
