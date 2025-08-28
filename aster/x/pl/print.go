//go:build slow

package pl

import prolog "mochi/aster/x/prolog"

// Print returns Prolog source code for the given Program.
func Print(p *Program) (string, error) {
	return prolog.Print((*prolog.Program)(p))
}
