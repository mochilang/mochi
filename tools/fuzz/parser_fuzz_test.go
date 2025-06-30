package fuzz

import (
	"math/rand"
	"testing"
	"time"

	"mochi/parser"
	"mochi/tools/fuzz/gen"
)

// FuzzParser ensures the parser can handle arbitrary input without panicking.
func FuzzParser(f *testing.F) {
	g := gen.New(rand.New(rand.NewSource(time.Now().UnixNano())))
	for i := 0; i < 50; i++ {
		f.Add(g.Program(3))
	}

	f.Fuzz(func(t *testing.T, src string) {
		// Errors are ignored; we only care about crashes.
		parser.ParseString(src)
	})
}
