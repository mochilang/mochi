//go:build slow

package main

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
)

func main() {
	cmd := exec.Command("git", "log", "--date=format:%Y-%m-%d %H:%M %Z", "--pretty=format:- %ad - %s", "transpiler/x/fortran")
	out, err := cmd.Output()
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	var buf bytes.Buffer
	buf.WriteString("# Transpiler Tasks\n")
	buf.WriteString("## Recent Enhancements\n")
	buf.Write(out)
	buf.WriteByte('\n')
	os.WriteFile("transpiler/x/fortran/TASKS.md", buf.Bytes(), 0644)
}
