package dartcode

import (
	"regexp"
	"strings"

	tscode "mochi/compile/ts"
	"mochi/parser"
	"mochi/types"
)

// Compiler translates a Mochi AST into Dart source code. The implementation
// reuses the TypeScript compiler and then performs a best-effort conversion of
// the generated TypeScript code into Dart syntax.
type Compiler struct {
	ts *tscode.Compiler
}

// New creates a new Dart compiler instance.
func New(env *types.Env) *Compiler {
	return &Compiler{ts: tscode.New(env)}
}

// Compile returns Dart source code implementing prog.
func (c *Compiler) Compile(prog *parser.Program) ([]byte, error) {
	code, err := c.ts.Compile(prog)
	if err != nil {
		return nil, err
	}
	src := string(code)
	src = strings.ReplaceAll(src, "Mochi TypeScript compiler", "Mochi Dart compiler")
	src = strings.ReplaceAll(src, "console.log", "print")
	src = strings.ReplaceAll(src, "let ", "var ")
	src = strings.ReplaceAll(src, "const ", "var ")
	src = strings.ReplaceAll(src, " of ", " in ")
	src = strings.ReplaceAll(src, ": any", "")
	src = strings.ReplaceAll(src, ": number", "")
	src = strings.ReplaceAll(src, ": string", "")
	src = strings.ReplaceAll(src, ": boolean", "")

	funcRe := regexp.MustCompile(`function ([a-zA-Z_][A-Za-z0-9_]*)\(([^)]*)\)(: [^ {]+)? {`)
	src = funcRe.ReplaceAllString(src, "dynamic $1($2) {")

	src = strings.ReplaceAll(src, "async function main(): Promise<void> {", "Future<void> main() async {")
	src = strings.ReplaceAll(src, "function main(): void {", "void main() {")
	src = strings.ReplaceAll(src, "await main()", "await main()")
	return []byte(src), nil
}
