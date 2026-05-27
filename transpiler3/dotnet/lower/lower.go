package lower

import (
	"path/filepath"
	"strings"
	"unicode"

	"mochi/transpiler3/c/aotir"
	"mochi/transpiler3/dotnet/colour"
	"mochi/transpiler3/dotnet/csharpsrc"
)

// Lower translates an aotir.Program into one CompilationUnit per type plus one
// for the main class. In Phase 0 this returns a single empty CompilationUnit as
// a skeleton; subsequent phases fill in the full translation.
func Lower(prog *aotir.Program, colours colour.ColourMap, className string) ([]*csharpsrc.CompilationUnit, error) {
	_ = prog
	_ = colours

	// Phase 0 stub: return a single empty compilation unit for the main class.
	cu := &csharpsrc.CompilationUnit{
		Namespace: "Mochi.User",
		Usings:    []string{"System"},
		Types: []csharpsrc.TypeDecl{
			&csharpsrc.ClassDecl{
				Modifiers: []string{"public", "static"},
				Name:      className,
			},
		},
	}
	return []*csharpsrc.CompilationUnit{cu}, nil
}

// ClassName converts a Mochi source filename to a PascalCase class name.
// "hello.mochi"      -> "Hello"
// "my_program.mochi" -> "MyProgram"
func ClassName(src string) string {
	// Strip directory prefix.
	src = filepath.Base(src)
	// Strip .mochi extension.
	src = strings.TrimSuffix(src, ".mochi")
	// Convert snake_case / kebab-case to PascalCase.
	parts := strings.FieldsFunc(src, func(r rune) bool {
		return r == '_' || r == '-'
	})
	var sb strings.Builder
	for _, p := range parts {
		if len(p) == 0 {
			continue
		}
		runes := []rune(p)
		runes[0] = unicode.ToUpper(runes[0])
		sb.WriteString(string(runes))
	}
	if sb.Len() == 0 {
		return "Main"
	}
	return sb.String()
}
