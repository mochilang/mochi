package any2mochi

// LanguageServer describes an external command used for parsing.
type LanguageServer struct {
	Command string
	Args    []string
	LangID  string
}

// Servers provides default commands per language. The map is intentionally
// empty after removing language server support but kept for API compatibility.
var Servers = map[string]LanguageServer{}
