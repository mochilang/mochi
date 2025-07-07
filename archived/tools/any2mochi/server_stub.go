package any2mochi

// LanguageServer represents an external parser command.
type LanguageServer struct {
	Command string
	Args    []string
	LangID  string
}

var Servers = map[string]LanguageServer{}
