package c

import (
	"bytes"
	"context"
	"encoding/json"
	"os/exec"
	"strings"
	"time"
)

// clangNode represents a subset of Clang's JSON AST.
type clangNode struct {
	Kind       string      `json:"kind"`
	Name       string      `json:"name"`
	Type       *clangType  `json:"type"`
	IsImplicit *bool       `json:"isImplicit"`
	Range      clangRange  `json:"range"`
	Inner      []clangNode `json:"inner"`
}

type clangType struct {
	QualType string `json:"qualType"`
}

type clangRange struct {
	Begin clangPos `json:"begin"`
	End   clangPos `json:"end"`
}

type clangPos struct {
	Offset int `json:"offset"`
}

// parseCFileClang parses C source code using clang's JSON AST output.
func parseCFileClang(src string) ([]cFunc, error) {
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	cmd := exec.CommandContext(ctx, "clang", "-w", "-x", "c", "-", "-Xclang", "-ast-dump=json", "-fsyntax-only")
	cmd.Stdin = strings.NewReader(src)
	var out bytes.Buffer
	cmd.Stdout = &out
	if err := cmd.Run(); err != nil {
		return nil, err
	}
	data := out.Bytes()
	if idx := bytes.IndexByte(data, '{'); idx > 0 {
		data = data[idx:]
	}
	var root clangNode
	if err := json.Unmarshal(data, &root); err != nil {
		return nil, err
	}
	var funcs []cFunc
	var walk func(n clangNode)
	walk = func(n clangNode) {
		if n.Kind == "FunctionDecl" && (n.IsImplicit == nil || !*n.IsImplicit) {
			if strings.Contains(n.Name, "_create") {
				return
			}
			ret := ""
			if n.Type != nil {
				typ := n.Type.QualType
				if open := strings.Index(typ, "("); open != -1 {
					ret = mapCType(strings.TrimSpace(typ[:open]))
				} else {
					ret = mapCType(typ)
				}
			}
			var params []cParam
			var body []string
			for _, c := range n.Inner {
				switch c.Kind {
				case "ParmVarDecl":
					name := c.Name
					t := ""
					if c.Type != nil {
						t = mapCType(c.Type.QualType)
					}
					params = append(params, cParam{name: name, typ: t})
				case "CompoundStmt":
					start := c.Range.Begin.Offset
					end := c.Range.End.Offset
					if start >= 0 && end <= len(src) && end > start {
						body = parseCStatements(src[start+1 : end])
					}
				}
			}
			if len(body) > 0 {
				funcs = append(funcs, cFunc{name: n.Name, ret: ret, params: params, body: body})
			}
		}
		for _, c := range n.Inner {
			walk(c)
		}
	}
	walk(root)
	return funcs, nil
}
