package fs

import (
	fsparse "mochi/tools/a2mochi/x/fs"
)

// Program represents a parsed F# source file.
// It mirrors the structure used by the any2mochi parser.
type Program = fsparse.Program

// Inspect parses the given F# source code using the official parser
// via the any2mochi helper and returns a Program describing the file.
func Inspect(src string) (*Program, error) {
	return fsparse.Parse(src)
}
