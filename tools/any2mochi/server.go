package any2mochi

import (
	"fmt"
	"os"
	"os/exec"
)

// LanguageServer describes how to invoke a language server for a given language.
type LanguageServer struct {
	Command string   // binary name
	Args    []string // additional command arguments
	LangID  string   // language id used by LSP
}

// Servers maps language names to their default language server configuration.
var Servers = map[string]LanguageServer{
	"go": {Command: "gopls", Args: nil, LangID: "go"},
	// python language server disabled by default
	"python":     {Command: "", Args: nil, LangID: "python"},
	"typescript": {Command: "typescript-language-server", Args: []string{"--stdio"}, LangID: "typescript"},
	"c":          {Command: "clangd", Args: []string{"--header-insertion=never"}, LangID: "c"},
	"cpp":        {Command: "clangd", Args: []string{"--header-insertion=never"}, LangID: "cpp"},
	"asm":        {Command: "asm-lsp", Args: nil, LangID: "asm"},
	"clj":        {Command: "clojure-lsp", Args: nil, LangID: "clojure"},
	"cobol":      {Command: "cobol-lsp", Args: nil, LangID: "cobol"},
	"jvm":        {Command: "jdtls", Args: nil, LangID: "jvm"},
	"cs":         {Command: "omnisharp", Args: []string{"-lsp"}, LangID: "csharp"},
	"dart":       {Command: "dart", Args: []string{"language-server"}, LangID: "dart"},
	"erlang":     {Command: "erlang_ls", Args: nil, LangID: "erlang"},
	"ex":         {Command: "elixir-ls", Args: nil, LangID: "elixir"},
	"fortran":    {Command: "fortls", Args: nil, LangID: "fortran"},
	"fs":         {Command: "fsautocomplete", Args: nil, LangID: "fsharp"},
	"hs":         {Command: "haskell-language-server-wrapper", Args: nil, LangID: "haskell"},
	"java":       {Command: "jdtls", Args: nil, LangID: "java"},
	"kt":         {Command: "kotlin-language-server", Args: nil, LangID: "kotlin"},
	"lua":        {Command: "lua-language-server", Args: nil, LangID: "lua"},
	"mlir":       {Command: "mlir-lsp-server", Args: nil, LangID: "mlir"},
	"ocaml":      {Command: "ocamllsp", Args: nil, LangID: "ocaml"},
	"pas":        {Command: "pasls", Args: nil, LangID: "pascal"},
	"php":        {Command: "intelephense", Args: []string{"--stdio"}, LangID: "php"},
	"prolog":     {Command: "prolog-lsp", Args: nil, LangID: "prolog"},
	"pl":         {Command: "perlls", Args: nil, LangID: "perl"},
	"rb":         {Command: "solargraph", Args: []string{"stdio"}, LangID: "ruby"},
	"rkt":        {Command: "racket-langserver", Args: nil, LangID: "racket"},
	"rust":       {Command: "rust-analyzer", Args: nil, LangID: "rust"},
	"scala":      {Command: "metals", Args: nil, LangID: "scala"},
	"scheme":     {Command: "scheme-langserver", Args: nil, LangID: "scheme"},
	"st":         {Command: "squeak", Args: []string{"--headless"}, LangID: "smalltalk"},
	"swift":      {Command: "sourcekit-lsp", Args: nil, LangID: "swift"},
	"wasm":       {Command: "", Args: nil, LangID: "wasm"},
	"zig":        {Command: "zls", Args: nil, LangID: "zig"},
}

// EnsureServer attempts to locate the given server binary and installs it using
// common package managers if missing. The installation step is best effort and
// errors only if the binary still cannot be found.
func EnsureServer(name string) error {
	if name == "" {
		return fmt.Errorf("no server specified")
	}
	if _, err := exec.LookPath(name); err == nil {
		return nil
	}
	if name == "rust-analyzer" {
		if _, err := exec.LookPath("rustup"); err == nil {
			cmd := exec.Command("rustup", "component", "add", "rust-analyzer")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
			if _, err := exec.LookPath(name); err == nil {
				return nil
			}
		}
	} else if name == "haskell-language-server-wrapper" {
		if _, err := exec.LookPath("ghcup"); err == nil {
			cmd := exec.Command("ghcup", "install", "hls")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
			if _, err := exec.LookPath(name); err == nil {
				return nil
			}
		}
		if _, err := exec.LookPath("apt-get"); err == nil {
			cmd := exec.Command("apt-get", "update")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
			cmd = exec.Command("apt-get", "install", "-y", "haskell-language-server")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
			if _, err := exec.LookPath(name); err == nil {
				return nil
			}
		}
		if _, err := exec.LookPath("brew"); err == nil {
			cmd := exec.Command("brew", "install", "haskell-language-server")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
			if _, err := exec.LookPath(name); err == nil {
				return nil
			}
		}
	}
	installers := []struct {
		bin  string
		args []string
	}{
		{"npm", []string{"install", "-g", name}},
		{"pnpm", []string{"add", "-g", name}},
		{"pip3", []string{"install", "--user", name}},
		{"pip", []string{"install", "--user", name}},
		{"go", []string{"install", name + "@latest"}},
		{"dotnet", []string{"tool", "install", "-g", name}},
		{"dotnet", []string{"tool", "update", "-g", name}},
		{"apt-get", []string{"install", "-y", name}},
		{"brew", []string{"install", name}},
		{"pacman", []string{"-Sy", name}},
	}
	for _, inst := range installers {
		if _, err := exec.LookPath(inst.bin); err == nil {
			cmd := exec.Command(inst.bin, inst.args...)
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
			if _, err := exec.LookPath(name); err == nil {
				return nil
			}
		}
	}
	if _, err := exec.LookPath(name); err == nil {
		return nil
	}
	return fmt.Errorf("%s not found", name)
}
