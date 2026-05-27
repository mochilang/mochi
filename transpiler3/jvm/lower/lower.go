package lower

import (
	"strings"
	"unicode"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/jvm/javasrc"
)

// Lower translates an aotir.Program to a javasrc.CompilationUnit.
func Lower(prog *aotir.Program, className string) (*javasrc.CompilationUnit, error) {
	mainFn := prog.Functions[prog.Main]

	body, err := lowerBlock(mainFn.Body)
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

	classDecl := &javasrc.ClassDecl{
		Modifiers: []string{"public", "final"},
		Name:      className,
		Members:   []javasrc.Member{mainMethod},
	}

	cu := &javasrc.CompilationUnit{
		Package: "dev.mochi.user",
		Types:   []javasrc.TypeDecl{classDecl},
	}
	return cu, nil
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
