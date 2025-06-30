package fuzz

// Generator provides example Mochi programs covering
// a wide portion of the AST. It can be used as a seed
// corpus for fuzzing the VM.
type Generator struct {
	idx      int
	programs []string
}

// NewGenerator returns a generator pre-populated with
// programs exercising different statement and expression
// forms. Each program is small but syntactically valid.
func NewGenerator() *Generator {
	return &Generator{programs: []string{
		// Declarations
		"let x = 1",
		"var y: int = 2",
		"x = x + 1",
		"fun add(a: int, b: int): int { return a + b }",
		"return 1",
		"type Pair { a: int, b: int }",
		"extern type Ext",
		"extern var math.pi: float",
		"extern fun math.sin(x: float): float",
		"extern object JS",
		"stream Data { value: int }",
		"model Person { name: \"\" }",
		"import \"math\" as math",
		// Control flow
		"if true { let z = 1 } else { var w = 2 }",
		"while false { break }",
		"for i in 0..3 { continue }",
		// Expressions
		"let l = [1, 2, 3]",
		"let m = { a: 1, b: 2 }",
		"let s = Point{ x: 1, y: 2 }",
		"let call = add(1, 2)",
		"let sel = s.x",
		"let unary = -1",
		"let binary = 1 + 2",
		"let idx = l[0]",
		"let slice = l[1:2]",
		"let cast = 1 as int",
		"let q = from x in l select x",
		"let mt = match 1 { 1 => 2 }",
		"let g = generate Person { name: \"bob\" }",
		"fetch \"https://example.com\" into resp",
		"load \"file.txt\" as int",
		"save 1 to \"out.txt\"",
		"emit Data { value: 1 }",
		// Agent DSL
		"agent Bot { on Data as d { print(d.value) } intent greet(u: string): string { return \"hi\" + u } }",
		// Logic DSL
		"fact parent(\"a\", \"b\")",
		"rule ancestor(a,b) :- parent(a,b)",
		// Testing
		"expect 1 == 1",
		"test \"t\" { expect 2 == 2 }",
	}}
}

// Next returns the next program source. ok is false when
// all programs have been exhausted.
func (g *Generator) Next() (src string, ok bool) {
	if g.idx >= len(g.programs) {
		return "", false
	}
	src = g.programs[g.idx]
	g.idx++
	return src, true
}
