package lower

import (
	"strings"
	"unicode"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/jvm/javasrc"
)

// lowerer carries compilation context shared across expression and statement
// lowering, specifically the className needed to emit static calls to
// user-defined functions in the same class.
type lowerer struct {
	className string
}

// Lower translates an aotir.Program to a javasrc.CompilationUnit.
func Lower(prog *aotir.Program, className string) (*javasrc.CompilationUnit, error) {
	l := &lowerer{className: className}

	mainFn := prog.Functions[prog.Main]

	body, err := l.lowerBlock(mainFn.Body)
	if err != nil {
		return nil, err
	}

	mainMethod := &javasrc.MethodDecl{
		Modifiers:  []string{"public", "static"},
		ReturnType: javasrc.TypeVoid,
		Name:       "main",
		Params:     []javasrc.Param{{Type: &javasrc.TypeRef{Name: "String", Array: true}, Name: "args"}},
		Body:       &body,
	}

	members := []javasrc.Member{mainMethod}

	// Emit user-defined functions as additional static methods.
	for i, fn := range prog.Functions {
		if i == prog.Main {
			continue
		}
		method, err := l.lowerFunction(fn)
		if err != nil {
			return nil, err
		}
		members = append(members, method)
	}

	classDecl := &javasrc.ClassDecl{
		Modifiers: []string{"public", "final"},
		Name:      className,
		Members:   members,
	}

	cu := &javasrc.CompilationUnit{
		Package: "dev.mochi.user",
		Types:   []javasrc.TypeDecl{classDecl},
	}
	return cu, nil
}

// lowerFunction translates a user-defined aotir.Function to a static MethodDecl.
func (l *lowerer) lowerFunction(fn *aotir.Function) (*javasrc.MethodDecl, error) {
	retType, err := lowerType(fn.ReturnType)
	if err != nil {
		return nil, err
	}

	params := make([]javasrc.Param, len(fn.Params))
	for i, p := range fn.Params {
		var pt javasrc.TypeRef
		switch p.Type {
		case aotir.TypeList:
			pt = lowerListType(p.ElemType)
		case aotir.TypeMap:
			pt = lowerMapType(p.KeyType, p.ValueType)
		case aotir.TypeSet:
			pt = lowerSetType(p.ElemType)
		default:
			pt, err = lowerType(p.Type)
			if err != nil {
				return nil, err
			}
		}
		params[i] = javasrc.Param{Type: &pt, Name: p.Name}
	}

	body, err := l.lowerBlock(fn.Body)
	if err != nil {
		return nil, err
	}

	return &javasrc.MethodDecl{
		Modifiers:  []string{"public", "static"},
		ReturnType: retType,
		Name:       fn.Name,
		Params:     params,
		Body:       &body,
	}, nil
}

// ClassName converts a Mochi source filename to a Java class name.
// "hello.mochi" -> "HelloMochi"
// "my_program.mochi" -> "MyProgramMochi"
func ClassName(src string) string {
	// Strip directory prefix
	for i := len(src) - 1; i >= 0; i-- {
		if src[i] == '/' || src[i] == '\\' {
			src = src[i+1:]
			break
		}
	}
	// Strip .mochi extension
	src = strings.TrimSuffix(src, ".mochi")
	// Convert snake_case to PascalCase
	parts := strings.Split(src, "_")
	var sb strings.Builder
	for _, p := range parts {
		if len(p) == 0 {
			continue
		}
		runes := []rune(p)
		runes[0] = unicode.ToUpper(runes[0])
		sb.WriteString(string(runes))
	}
	sb.WriteString("Mochi")
	return sb.String()
}
