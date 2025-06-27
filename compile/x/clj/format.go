package cljcode

import (
	"bytes"
	"os/exec"
)

// formatClojure uses cljfmt for pretty printing if the clojure
// command is available. If formatting fails, the original code is returned.
func formatClojure(src []byte) []byte {
	runner := "clojure"
	if _, err := exec.LookPath(runner); err != nil {
		if _, err := exec.LookPath("clj"); err == nil {
			runner = "clj"
		} else {
			return src
		}
	}
	cmd := exec.Command(runner, "-Sdeps", "{:deps {cljfmt {:mvn/version \"0.9.2\"}}}", "-M", "-m", "cljfmt.main")
	cmd.Stdin = bytes.NewReader(src)
	out, err := cmd.Output()
	if err != nil {
		return src
	}
	return out
}
