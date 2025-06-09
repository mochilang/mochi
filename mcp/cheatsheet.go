package mcp

import _ "embed"

//go:embed cheatsheet.mochi
var cheatsheet string

// Cheatsheet returns the embedded Mochi language cheatsheet.
func Cheatsheet() string { return cheatsheet }
