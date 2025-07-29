//go:build slow

package dart

import (
	a2dart "mochi/tools/a2mochi/x/dart"
)

// Program represents a parsed Dart source file.
type Program struct {
	Functions []Function `json:"functions"`
	Classes   []Class    `json:"classes"`
}

// Function represents a Dart function.
type Function struct {
	Name   string   `json:"name"`
	Params []Param  `json:"params"`
	Ret    string   `json:"ret"`
	Body   []string `json:"body"`
	Start  int      `json:"start"`
	End    int      `json:"end"`
	Doc    string   `json:"doc,omitempty"`
}

// Param represents a function parameter.
type Param struct {
	Name string `json:"name"`
	Type string `json:"type"`
}

// Field represents a class field.
type Field struct {
	Name string `json:"name"`
	Type string `json:"type"`
}

// Class represents a Dart class definition.
type Class struct {
	Name   string  `json:"name"`
	Fields []Field `json:"fields"`
	Start  int     `json:"start"`
	End    int     `json:"end"`
	Doc    string  `json:"doc,omitempty"`
}

// Inspect parses Dart source code and returns its Program structure.
func Inspect(src string) (*Program, error) {
	p, err := a2dart.Parse(src)
	if err != nil {
		return nil, err
	}
	// convert to local Program struct to avoid exposing Src field
	out := &Program{
		Functions: make([]Function, len(p.Functions)),
		Classes:   make([]Class, len(p.Classes)),
	}
	for i, fn := range p.Functions {
		params := make([]Param, len(fn.Params))
		for j, pm := range fn.Params {
			params[j] = Param{Name: pm.Name, Type: pm.Type}
		}
		body := append([]string(nil), fn.Body...)
		out.Functions[i] = Function{
			Name:   fn.Name,
			Params: params,
			Ret:    fn.Ret,
			Body:   body,
			Start:  fn.Start,
			End:    fn.End,
			Doc:    fn.Doc,
		}
	}
	for i, c := range p.Classes {
		fields := make([]Field, len(c.Fields))
		for j, f := range c.Fields {
			fields[j] = Field{Name: f.Name, Type: f.Type}
		}
		out.Classes[i] = Class{
			Name:   c.Name,
			Fields: fields,
			Start:  c.Start,
			End:    c.End,
			Doc:    c.Doc,
		}
	}
	return out, nil
}
