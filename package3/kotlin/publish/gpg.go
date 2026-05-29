package publish

import "os/exec"

// execGPG returns an exec.Cmd for running gpg with args.
// Separated to allow tests to override GPG behavior.
var execGPG = func(args ...string) *exec.Cmd {
	return exec.Command("gpg", args...)
}
