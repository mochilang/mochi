package lower

// goReserved is the set of Go reserved words plus a small set
// of predeclared identifiers we refuse to shadow in emitted
// code. Phase 0 ships the table; later phases call mangleIdent
// to rewrite Mochi identifiers that collide.
var goReserved = map[string]struct{}{
	"break":       {},
	"case":        {},
	"chan":        {},
	"const":       {},
	"continue":    {},
	"default":     {},
	"defer":       {},
	"else":        {},
	"fallthrough": {},
	"for":         {},
	"func":        {},
	"go":          {},
	"goto":        {},
	"if":          {},
	"import":      {},
	"interface":   {},
	"map":         {},
	"package":     {},
	"range":       {},
	"return":      {},
	"select":      {},
	"struct":      {},
	"switch":      {},
	"type":        {},
	"var":         {},
	"any":         {},
	"true":        {},
	"false":       {},
	"nil":         {},
	"iota":        {},
}

// mangleIdent rewrites a Mochi identifier into a Go identifier
// safe from reserved-word and predeclared-name shadowing. The
// rewrite is reversible: every mangled name has a unique
// pre-image, so the lowerer can round-trip names through
// emit/disassemble paths in test fixtures.
func mangleIdent(name string) string {
	if _, bad := goReserved[name]; bad {
		return name + "_"
	}
	return name
}
