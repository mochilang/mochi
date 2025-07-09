//go:build slow

package cpp

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os/exec"
	"regexp"
	"strings"
)

func lineNumber(src string, offset int) int {
	if offset > len(src) {
		offset = len(src)
	}
	return strings.Count(src[:offset], "\n") + 1
}

type funcDef struct {
	name      string
	params    []param
	ret       string
	body      string
	startLine int
	endLine   int
}

type enumDef struct {
	name     string
	variants []string
}

type structField struct {
	name string
	typ  string
}

type structDef struct {
	name      string
	fields    []structField
	methods   []funcDef
	startLine int
	endLine   int
	snippet   string
}

func convertBodyString(body string) []string {
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
		case strings.HasPrefix(l, "for (") && strings.Contains(l, ":"):
			re := regexp.MustCompile(`^for \((?:const\s+)?(?:auto|[\w:<>,]+)[\s*&]*([A-Za-z_][A-Za-z0-9_]*)\s*:\s*([^\)]+)\)\s*\{?$`)
			if m := re.FindStringSubmatch(l); m != nil {
				name := m[1]
				src := strings.TrimSpace(m[2])
				out = append(out, fmt.Sprintf("for %s in %s {", name, src))
			} else {
				out = append(out, l)
			}
		case strings.HasPrefix(l, "while ("):
			if m := regexp.MustCompile(`^while \((.*)\)\s*\{?$`).FindStringSubmatch(l); m != nil {
				cond := strings.TrimSpace(m[1])
				out = append(out, fmt.Sprintf("while %s {", cond))
			} else {
				out = append(out, l)
			}
		case strings.HasPrefix(l, "if ("):
			if m := regexp.MustCompile(`^if \((.*)\)\s*\{?$`).FindStringSubmatch(l); m != nil {
				cond := strings.TrimSpace(m[1])
				out = append(out, fmt.Sprintf("if %s {", cond))
			} else {
				out = append(out, l)
			}
		default:
			decl := false
			if strings.HasPrefix(l, "const ") {
				l = strings.TrimPrefix(l, "const ")
			}
			for _, pre := range []string{"int ", "float ", "double ", "long ", "unsigned ", "bool ", "std::string ", "string ", "auto "} {
				if strings.HasPrefix(l, pre) {
					l = strings.TrimPrefix(l, pre)
					decl = true
					break
				}
			}
			if !decl {
				for _, pre := range []string{"std::vector<", "vector<", "std::map<", "std::unordered_map<", "map<", "unordered_map<", "std::set<", "set<", "std::array<", "array<"} {
					if strings.HasPrefix(l, pre) {
						if idx := strings.Index(l, ">"); idx != -1 {
							l = strings.TrimSpace(l[idx+1:])
							decl = true
						}
						break
					}
				}
			}
			if decl {
				l = "let " + l
			}
			out = append(out, l)
		}
	}
	return out
}

// parseAST uses clang++ to produce a JSON AST and extracts functions and enums.
func parseAST(src string) ([]funcDef, []enumDef, []structDef, error) {
	cmd := exec.Command("clang++", "-x", "c++", "-std=c++20", "-fsyntax-only", "-Xclang", "-ast-dump=json", "-")
	cmd.Stdin = strings.NewReader(src)
	var buf bytes.Buffer
	var errBuf bytes.Buffer
	cmd.Stdout = &buf
	cmd.Stderr = &errBuf
	runErr := cmd.Run()
	if runErr != nil && buf.Len() == 0 {
		return nil, nil, nil, fmt.Errorf("clang++: %w: %s", runErr, errBuf.String())
	}

	var root astNode
	if err := json.Unmarshal(buf.Bytes(), &root); err != nil {
		if runErr != nil {
			return nil, nil, nil, fmt.Errorf("%v: %w", runErr, err)
		}
		return nil, nil, nil, err
	}
	var funcs []funcDef
	var enums []enumDef
	var structs []structDef
	collectAST(&root, src, &funcs, &enums, &structs, "")
	if runErr != nil {
		return funcs, enums, structs, fmt.Errorf("clang++: %w: %s", runErr, errBuf.String())
	}
	return funcs, enums, structs, nil
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

func collectAST(n *astNode, src string, funcs *[]funcDef, enums *[]enumDef, structs *[]structDef, parent string) {
	switch n.Kind {
	case "FunctionDecl":
		if n.Range == nil || n.Range.Begin.Offset < 0 || n.Range.End.Offset > len(src) {
			break
		}
		var params []param
		var body string
		for i := range n.Inner {
			c := &n.Inner[i]
			switch c.Kind {
			case "ParmVarDecl":
				typ := ""
				if c.Type != nil {
					typ = mapType(c.Type.QualType)
				}
				params = append(params, param{name: c.Name, typ: typ})
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
			*funcs = append(*funcs, funcDef{
				name:      n.Name,
				params:    params,
				ret:       ret,
				body:      body,
				startLine: lineNumber(src, n.Range.Begin.Offset),
				endLine:   lineNumber(src, n.Range.End.Offset),
			})
		}
	case "CXXMethodDecl":
		if parent == "" || n.Range == nil || n.Range.Begin.Offset < 0 || n.Range.End.Offset > len(src) {
			break
		}
		var params []param
		var body string
		for i := range n.Inner {
			c := &n.Inner[i]
			switch c.Kind {
			case "ParmVarDecl":
				typ := ""
				if c.Type != nil {
					typ = mapType(c.Type.QualType)
				}
				params = append(params, param{name: c.Name, typ: typ})
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
		name := parent + "." + n.Name
		fn := funcDef{
			name:      name,
			params:    params,
			ret:       ret,
			body:      body,
			startLine: lineNumber(src, n.Range.Begin.Offset),
			endLine:   lineNumber(src, n.Range.End.Offset),
		}
		*funcs = append(*funcs, fn)
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
			*enums = append(*enums, enumDef{name: n.Name, variants: vars})
		}
	case "CXXRecordDecl", "RecordDecl":
		if n.Name != "" && n.Range != nil && n.Range.Begin.Offset >= 0 && n.Range.End.Offset <= len(src) {
			snippet := src[n.Range.Begin.Offset:n.Range.End.Offset]
			if strings.Contains(snippet, "struct "+n.Name) || strings.Contains(snippet, "class "+n.Name) {
				var fields []structField
				var methods []funcDef
				for i := range n.Inner {
					c := &n.Inner[i]
					switch c.Kind {
					case "FieldDecl":
						if c.Type != nil {
							fields = append(fields, structField{name: c.Name, typ: mapType(c.Type.QualType)})
						}
					case "CXXMethodDecl":
						var params []param
						var body string
						for j := range c.Inner {
							ch := &c.Inner[j]
							switch ch.Kind {
							case "ParmVarDecl":
								typ := ""
								if ch.Type != nil {
									typ = mapType(ch.Type.QualType)
								}
								params = append(params, param{name: ch.Name, typ: typ})
							case "CompoundStmt":
								if ch.Range != nil {
									b := ch.Range.Begin.Offset
									e := ch.Range.End.Offset
									if b >= 0 && e >= b && e <= len(src) {
										body = src[b:e]
									}
								}
							}
						}
						ret := ""
						if c.Type != nil {
							qt := c.Type.QualType
							if p := strings.Index(qt, "("); p != -1 {
								ret = qt[:p]
							}
							ret = mapType(strings.TrimSpace(ret))
						}
						fn := funcDef{
							name:      n.Name + "." + c.Name,
							params:    params,
							ret:       ret,
							body:      body,
							startLine: lineNumber(src, c.Range.Begin.Offset),
							endLine:   lineNumber(src, c.Range.End.Offset),
						}
						methods = append(methods, fn)
						*funcs = append(*funcs, fn)
					}
				}
				if len(fields) > 0 || len(methods) > 0 {
					*structs = append(*structs, structDef{
						name:      n.Name,
						fields:    fields,
						methods:   methods,
						startLine: lineNumber(src, n.Range.Begin.Offset),
						endLine:   lineNumber(src, n.Range.End.Offset),
						snippet:   snippet,
					})
				}
			}
		}
	}
	for i := range n.Inner {
		nextParent := parent
		if n.Kind == "CXXRecordDecl" || n.Kind == "RecordDecl" {
			if n.Name != "" {
				nextParent = n.Name
			}
		}
		if n.Inner[i].Kind == "CXXMethodDecl" {
			// already processed above
			continue
		}
		collectAST(&n.Inner[i], src, funcs, enums, structs, nextParent)
	}
}
