package c

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os/exec"
	"strings"
)

// Program represents the high level structure of a C source file.
type Program struct {
	Funcs   []Func   `json:"funcs,omitempty"`
	Enums   []Enum   `json:"enums,omitempty"`
	Structs []Struct `json:"structs,omitempty"`
	Globals []Global `json:"globals,omitempty"`
}

type Func struct {
	Name   string  `json:"name"`
	Params []Param `json:"params,omitempty"`
	Ret    string  `json:"ret,omitempty"`
	Body   string  `json:"body,omitempty"`
}

type Param struct {
	Name string `json:"name"`
	Typ  string `json:"type"`
}

type Enum struct {
	Name     string   `json:"name"`
	Variants []string `json:"variants"`
}

type Struct struct {
	Name   string  `json:"name"`
	Fields []Field `json:"fields"`
}

type Field struct {
	Name string `json:"name"`
	Typ  string `json:"type"`
}

type Global struct {
	Name  string `json:"name"`
	Typ   string `json:"type"`
	Value string `json:"value,omitempty"`
}

// Inspect parses a C source string using clang and returns a simplified Program.
func Inspect(src string) (*Program, error) {
	funcs, enums, structs, globals, err := parseClangAST(src)
	if err != nil {
		return nil, err
	}
	return &Program{Funcs: funcs, Enums: enums, Structs: structs, Globals: globals}, nil
}

type clangNode struct {
	Kind string `json:"kind"`
	Name string `json:"name,omitempty"`
	Type *struct {
		QualType string `json:"qualType"`
	} `json:"type,omitempty"`
	Inner []clangNode     `json:"inner,omitempty"`
	Value json.RawMessage `json:"value,omitempty"`
	Range *struct {
		Begin struct {
			Offset int `json:"offset"`
		} `json:"begin"`
		End struct {
			Offset int `json:"offset"`
		} `json:"end"`
	} `json:"range,omitempty"`
}

func parseClangAST(src string) ([]Func, []Enum, []Struct, []Global, error) {
	cmd := exec.Command("clang", "-x", "c", "-fsyntax-only", "-Xclang", "-ast-dump=json", "-")
	cmd.Stdin = strings.NewReader(src)
	var buf bytes.Buffer
	var errBuf bytes.Buffer
	cmd.Stdout = &buf
	cmd.Stderr = &errBuf
	runErr := cmd.Run()
	if buf.Len() == 0 {
		return nil, nil, nil, nil, fmt.Errorf("clang: %w: %s", runErr, errBuf.String())
	}
	var root clangNode
	if err := json.Unmarshal(buf.Bytes(), &root); err != nil {
		if runErr != nil {
			return nil, nil, nil, nil, fmt.Errorf("%v: %w", runErr, err)
		}
		return nil, nil, nil, nil, err
	}
	var funcs []Func
	var enums []Enum
	var structs []Struct
	var globals []Global
	walkAST(&root, src, &funcs, &enums, &structs, &globals)
	return funcs, enums, structs, globals, nil
}

func walkAST(n *clangNode, src string, funcs *[]Func, enums *[]Enum, structs *[]Struct, globals *[]Global) {
	switch n.Kind {
	case "FunctionDecl":
		if n.Range == nil || n.Range.Begin.Offset <= 0 || n.Range.End.Offset > len(src) {
			break
		}
		var params []Param
		var body string
		for i := range n.Inner {
			c := &n.Inner[i]
			switch c.Kind {
			case "ParmVarDecl":
				typ := ""
				if c.Type != nil {
					typ = mapType(c.Type.QualType)
				}
				params = append(params, Param{Name: c.Name, Typ: typ})
			case "CompoundStmt":
				if c.Range != nil {
					b := c.Range.Begin.Offset
					e := c.Range.End.Offset
					if b >= 0 && e >= b && e <= len(src) {
						body = src[b:e]
					}
				}
			}
		}
		ret := ""
		if n.Type != nil {
			qt := n.Type.QualType
			if p := strings.Index(qt, "("); p != -1 {
				ret = qt[:p]
			}
			ret = mapType(strings.TrimSpace(ret))
		}
		if body != "" {
			*funcs = append(*funcs, Func{Name: n.Name, Params: params, Ret: ret, Body: body})
		}

	case "VarDecl":
		if n.Range != nil && n.Range.Begin.Offset > 0 && n.Range.End.Offset <= len(src) {
			start := n.Range.Begin.Offset
			end := n.Range.End.Offset
			if end <= len(src) {
				snippet := src[start:end]
				if strings.Count(snippet, "{") > strings.Count(snippet, "}") {
					if idx := strings.Index(src[end:], ";"); idx != -1 {
						end += idx + 1
					}
				}
			}
			if end > len(src) {
				end = len(src)
			}
			snippet := strings.TrimSpace(src[start:end])
			g := parseGlobalDecl(snippet)
			if g.Name != "" {
				if len(n.Inner) > 0 && n.Inner[0].Kind == "IntegerLiteral" {
					var lit string
					json.Unmarshal(n.Inner[0].Value, &lit)
					g.Value = strings.TrimSpace(lit)
				}
				*globals = append(*globals, g)
			}
		}

	case "EnumDecl":
		if n.Range != nil && n.Range.Begin.Offset > 0 && n.Range.End.Offset <= len(src) {
			var vars []string
			for i := range n.Inner {
				c := &n.Inner[i]
				if c.Kind == "EnumConstantDecl" {
					vars = append(vars, c.Name)
				}
			}
			if len(vars) > 0 && n.Name != "" {
				*enums = append(*enums, Enum{Name: n.Name, Variants: vars})
			}
		}

	case "RecordDecl":
		if n.Name != "" && n.Range != nil && n.Range.Begin.Offset > 0 && n.Range.End.Offset <= len(src) {
			snippet := src[n.Range.Begin.Offset:n.Range.End.Offset]
			if strings.Contains(snippet, "struct "+n.Name) {
				var fields []Field
				for i := range n.Inner {
					c := &n.Inner[i]
					if c.Kind == "FieldDecl" && c.Type != nil {
						fields = append(fields, Field{Name: c.Name, Typ: mapType(c.Type.QualType)})
					}
				}
				if len(fields) > 0 {
					*structs = append(*structs, Struct{Name: n.Name, Fields: fields})
				}
			}
		}
	}
	for i := range n.Inner {
		walkAST(&n.Inner[i], src, funcs, enums, structs, globals)
	}
}

func parseGlobalDecl(s string) Global {
	s = strings.TrimSpace(strings.TrimSuffix(s, ";"))
	parts := strings.SplitN(s, "=", 2)
	left := strings.TrimSpace(parts[0])
	var val string
	if len(parts) > 1 {
		val = strings.TrimSpace(parts[1])
		val = strings.TrimSuffix(strings.TrimPrefix(val, "("), ")")
	}
	fields := strings.Fields(left)
	if len(fields) == 0 {
		return Global{}
	}
	name := fields[len(fields)-1]
	typToken := strings.Join(fields[:len(fields)-1], " ")
	typ := mapType(typToken)
	if (typ == "int" && val == "0") || (typ == "float" && (val == "0" || val == "0.0")) || (typ == "bool" && val == "false") || (typ == "string" && val == "\"\"") {
		val = ""
	}
	return Global{Name: name, Typ: typ, Value: val}
}

func mapType(typ string) string {
	typ = strings.TrimSpace(typ)
	for strings.HasSuffix(typ, "*") || strings.HasSuffix(typ, "&") {
		typ = strings.TrimSpace(typ[:len(typ)-1])
	}
	typ = strings.TrimPrefix(typ, "const ")
	typ = strings.TrimPrefix(typ, "unsigned ")
	switch typ {
	case "", "void":
		return ""
	case "int", "size_t", "long", "short", "long long":
		return "int"
	case "float", "double":
		return "float"
	case "bool":
		return "bool"
	case "auto":
		return ""
	case "char", "char16_t", "char32_t", "std::string", "string":
		return "string"
	}
	if strings.HasPrefix(typ, "char") {
		return "int"
	}
	return typ
}
