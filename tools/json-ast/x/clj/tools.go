package clj

// This file previously handled installation of the Babashka tool when parsing
// was implemented using Babashka. The parser now relies on tree-sitter, so the
// helper remains as a no-op for compatibility.

import "fmt"

// EnsureBabashka verifies that the babashka command is available. It attempts a
// best-effort installation using common package managers when missing.
func EnsureBabashka() error {
	// No longer needed; always succeed.
	fmt.Println("Babashka no longer required; using tree-sitter parser")
	return nil
}
