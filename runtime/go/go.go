package goexec

import "os/exec"

// Bin specifies the Go binary to use. It defaults to "go".
var Bin = "go"

// Command returns an exec.Cmd that runs the Go tool with the given arguments.
func Command(args ...string) *exec.Cmd {
	return exec.Command(Bin, args...)
}
