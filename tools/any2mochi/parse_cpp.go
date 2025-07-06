package any2mochi

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os/exec"
	"strings"
)

type cppFuncDef struct {
	name   string
	params []cppParam
	ret    string
	body   string
}

type cppEnumDef struct {
	name     string
	variants []string
}

func convertCppBodyString(body string) []string {
	lines := strings.Split(body, "\n")
	if len(lines) > 0 && strings.TrimSpace(lines[0]) == "{" {
		lines = lines[1:]
	}
	if len(lines) > 0 && strings.TrimSpace(lines[len(lines)-1]) == "}" {
		lines = lines[:len(lines)-1]
	}
	var out []string
	for _, l := range lines {
		l = strings.TrimSpace(l)
		l = strings.TrimSuffix(l, ";")
		if l == "" {
			continue
		}
		switch {
		case strings.HasPrefix(l, "return"):
			out = append(out, l)
		case strings.Contains(l, "std::cout") || strings.HasPrefix(l, "cout <<"):
			l = strings.TrimPrefix(l, "std::cout <<")
			l = strings.TrimPrefix(l, "cout <<")
			l = strings.TrimSuffix(l, "<< std::endl")
			l = strings.TrimSuffix(l, "<< endl")
			l = strings.TrimSpace(l)
			out = append(out, "print("+l+")")
		default:
			for _, pre := range []string{"int ", "float ", "double ", "bool ", "std::string ", "string ", "auto "} {
				if strings.HasPrefix(l, pre) {
					l = strings.TrimPrefix(l, pre)
					break
				}
			}
			out = append(out, l)
		}
	}
	return out
}

// parseCppAST uses clang++ to produce a JSON AST and extracts functions and enums.
func parseCppAST(src string) ([]cppFuncDef, []cppEnumDef, error) {
	cmd := exec.Command("clang++", "-x", "c++", "-std=c++20", "-fsyntax-only", "-Xclang", "-ast-dump=json", "-")
	cmd.Stdin = strings.NewReader(src)
	var buf bytes.Buffer
	var errBuf bytes.Buffer
	cmd.Stdout = &buf
	cmd.Stderr = &errBuf
	if err := cmd.Run(); err != nil {
		return nil, nil, fmt.Errorf("clang++: %w: %s", err, errBuf.String())
	}

	var root astNode
	if err := json.Unmarshal(buf.Bytes(), &root); err != nil {
		return nil, nil, err
	}
	var funcs []cppFuncDef
	var enums []cppEnumDef
	collectCppAST(&root, src, &funcs, &enums)
	return funcs, enums, nil
}

type astNode struct {
	Kind string `json:"kind"`
	Name string `json:"name,omitempty"`
	Type *struct {
		QualType string `json:"qualType"`
	} `json:"type,omitempty"`
	Inner []astNode `json:"inner,omitempty"`
	Range *struct {
		Begin struct {
			Offset int `json:"offset"`
		} `json:"begin"`
		End struct {
			Offset int `json:"offset"`
		} `json:"end"`
	} `json:"range,omitempty"`
}

func collectCppAST(n *astNode, src string, funcs *[]cppFuncDef, enums *[]cppEnumDef) {
	switch n.Kind {
	case "FunctionDecl":
		if n.Range == nil || n.Range.Begin.Offset < 0 || n.Range.End.Offset > len(src) {
			break
		}
		var params []cppParam
		var body string
		for i := range n.Inner {
			c := &n.Inner[i]
			switch c.Kind {
			case "ParmVarDecl":
				typ := ""
				if c.Type != nil {
					typ = mapCppType(c.Type.QualType)
				}
				params = append(params, cppParam{name: c.Name, typ: typ})
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
			ret = mapCppType(strings.TrimSpace(ret))
		}
		if body != "" {
			*funcs = append(*funcs, cppFuncDef{name: n.Name, params: params, ret: ret, body: body})
		}
	case "EnumDecl":
		if n.Range == nil || n.Range.Begin.Offset < 0 || n.Range.End.Offset > len(src) {
			break
		}
		var vars []string
		for i := range n.Inner {
			c := &n.Inner[i]
			if c.Kind == "EnumConstantDecl" {
				vars = append(vars, c.Name)
			}
		}
		if len(vars) > 0 && n.Name != "" {
			*enums = append(*enums, cppEnumDef{name: n.Name, variants: vars})
		}
	}
	for i := range n.Inner {
		collectCppAST(&n.Inner[i], src, funcs, enums)
	}
}
