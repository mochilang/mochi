//go:build archive && slow

package main

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
)

func main() {
	cmd := exec.Command("git", "log", "--date=iso", "--pretty=format:- %ad - %s", "archived/transpiler/x/st")
	out, err := cmd.Output()
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	var buf bytes.Buffer
	buf.WriteString("# Smalltalk Transpiler Tasks\n")
	buf.WriteString("## History\n")
	buf.Write(out)
	buf.WriteByte('\n')
	os.WriteFile("archived/transpiler/x/st/TASKS.md", buf.Bytes(), 0644)
}
