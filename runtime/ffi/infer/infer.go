package infer

// SymbolKind describes the kind of a symbol exposed by a foreign module.
type SymbolKind string

const (
	SymbolFunc      SymbolKind = "func"
	SymbolVar       SymbolKind = "var"
	SymbolConst     SymbolKind = "const"
	SymbolStruct    SymbolKind = "struct"
	SymbolInterface SymbolKind = "interface"
	SymbolType      SymbolKind = "type"
)

// Symbol represents an exported value from a foreign module.
type Symbol struct {
	Name string
	Kind SymbolKind
	Type string
}

// Package describes the exported API of a module.
type Package struct {
	Name    string
	Symbols []Symbol
}
