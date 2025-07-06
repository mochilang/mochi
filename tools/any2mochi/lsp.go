package any2mochi

// Basic types mirroring the subset of LSP structures used by converters.

type Position struct {
	Line      int `json:"line"`
	Character int `json:"character"`
}

type Range struct {
	Start Position `json:"start"`
	End   Position `json:"end"`
}

type DocumentSymbol struct {
	Name           string           `json:"name"`
	Detail         *string          `json:"detail,omitempty"`
	Kind           int              `json:"kind"`
	Range          Range            `json:"range"`
	SelectionRange Range            `json:"selectionRange"`
	Deprecated     bool             `json:"deprecated,omitempty"`
	Tags           []int            `json:"tags,omitempty"`
	Children       []DocumentSymbol `json:"children,omitempty"`
}

type Diagnostic struct {
	Range    Range  `json:"range"`
	Message  string `json:"message"`
	Severity int    `json:"severity,omitempty"`
	Source   string `json:"source,omitempty"`
}

type MarkupContent struct {
	Kind  string `json:"kind"`
	Value string `json:"value"`
}

type MarkedString struct {
	Language string `json:"language,omitempty"`
	Value    string `json:"value"`
}

type MarkedStringStruct struct {
	Language string `json:"language"`
	Value    string `json:"value"`
}

type Hover struct {
	Contents any `json:"contents"`
}

type Location struct {
	URI   string `json:"uri"`
	Range Range  `json:"range"`
}

// SymbolKind values used by the converters.
const (
	SymbolKindFile        = 1
	SymbolKindModule      = 2
	SymbolKindNamespace   = 3
	SymbolKindPackage     = 4
	SymbolKindClass       = 5
	SymbolKindMethod      = 6
	SymbolKindProperty    = 7
	SymbolKindField       = 8
	SymbolKindConstructor = 9
	SymbolKindEnum        = 10
	SymbolKindInterface   = 11
	SymbolKindFunction    = 12
	SymbolKindVariable    = 13
	SymbolKindConstant    = 14
	SymbolKindString      = 15
	SymbolKindNumber      = 16
	SymbolKindBoolean     = 17
	SymbolKindArray       = 18
	SymbolKindObject      = 19
	SymbolKindKey         = 20
	SymbolKindNull        = 21
	SymbolKindEnumMember  = 24
	SymbolKindStruct      = 23
)
