package any2mochi

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os/exec"
	"regexp"
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

var cppFuncRE = regexp.MustCompile(`(?m)([A-Za-z_][A-Za-z0-9_:<>*&\s]*)\s+([A-Za-z_][A-Za-z0-9_]*)\s*\(([^)]*)\)\s*\{`)
var cppEnumRE = regexp.MustCompile(`(?m)enum\s+(?:class\s+)?([A-Za-z_][A-Za-z0-9_]*)\s*\{`)

func parseCppFunctions(src string) []cppFuncDef {
	var funcs []cppFuncDef
	idxs := cppFuncRE.FindAllStringSubmatchIndex(src, -1)
	for _, m := range idxs {
		retPart := strings.TrimSpace(src[m[2]:m[3]])
		name := strings.TrimSpace(src[m[4]:m[5]])
		paramsPart := strings.TrimSpace(src[m[6]:m[7]])
		open := m[1] - 1
		close := findMatch(src, open, '{', '}')
		if close <= open {
			continue
		}
		body := src[open+1 : close]
		sig := retPart + " " + name + "(" + paramsPart + ")"
		params, ret := parseCppSignature(sig)
		funcs = append(funcs, cppFuncDef{name: name, params: params, ret: ret, body: body})
	}
	return funcs
}

func parseCppEnums(src string) []cppEnumDef {
	var enums []cppEnumDef
	idxs := cppEnumRE.FindAllStringSubmatchIndex(src, -1)
	for _, m := range idxs {
		name := strings.TrimSpace(src[m[2]:m[3]])
		open := m[1] - 1
		close := findMatch(src, open, '{', '}')
		if close <= open {
			continue
		}
		body := src[open+1 : close]
		parts := strings.Split(body, ",")
		var vars []string
		for _, p := range parts {
			v := strings.TrimSpace(p)
			if v == "" {
				continue
			}
			if eq := strings.Index(v, "="); eq != -1 {
				v = strings.TrimSpace(v[:eq])
			}
			v = strings.TrimSuffix(v, "}")
			v = strings.TrimSpace(v)
			if v != "" {
				vars = append(vars, v)
			}
		}
		enums = append(enums, cppEnumDef{name: name, variants: vars})
	}
	return enums
}

func convertCppBodyString(body string) []string {
	lines := strings.Split(body, "\n")
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
		*funcs = append(*funcs, cppFuncDef{name: n.Name, params: params, ret: ret, body: body})
	case "EnumDecl":
		var vars []string
		for i := range n.Inner {
			c := &n.Inner[i]
			if c.Kind == "EnumConstantDecl" {
				vars = append(vars, c.Name)
			}
		}
		*enums = append(*enums, cppEnumDef{name: n.Name, variants: vars})
	}
	for i := range n.Inner {
		collectCppAST(&n.Inner[i], src, funcs, enums)
	}
}
