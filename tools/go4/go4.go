//go:build interpreter

package main

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
)

func compile(src, out string, asm bool) error {
	switch filepath.Ext(src) {
	case ".c":
		var cmd *exec.Cmd
		if asm {
			cmd = exec.Command("gcc", "-w", "-S", src, "-o", out)
		} else {
			cmd = exec.Command("gcc", "-w", "-o", out, src)
		}
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		return cmd.Run()
	case ".go":
		if asm {
			cmd := exec.Command("go", "tool", "compile", "-S", src)
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			return cmd.Run()
		}
		cmd := exec.Command("go", "build", "-tags=interpreter", "-o", out, src)
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		return cmd.Run()
	default:
		return fmt.Errorf("unsupported source: %s", src)
	}
}

func runBinary(path string, args ...string) error {
	cmd := exec.Command(path, args...)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	return cmd.Run()
}

func main() {
	args := os.Args[1:]
	showAsm := false
	if len(args) > 0 && args[0] == "-s" {
		showAsm = true
		args = args[1:]
	}
	if len(args) == 0 {
		fmt.Println("usage: go4 [-s] file [file ...]")
		return
	}
	tmpDir, err := os.MkdirTemp("", "go4-")
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		return
	}
	defer os.RemoveAll(tmpDir)
	exe := filepath.Join(tmpDir, "prog0")
	if err := compile(args[0], exe, showAsm); err != nil {
		fmt.Fprintln(os.Stderr, err)
		return
	}
	if len(args) == 1 {
		if !showAsm {
			_ = runBinary(exe)
		}
		return
	}
	prev := exe
	for i, src := range args[1:] {
		if err := runBinary(prev, src); err != nil {
			fmt.Fprintln(os.Stderr, err)
			return
		}
		if i < len(args)-2 {
			next := filepath.Join(tmpDir, fmt.Sprintf("prog%d", i+1))
			if err := os.Rename("a.out", next); err != nil {
				fmt.Fprintln(os.Stderr, err)
				return
			}
			prev = next
		}
	}
}
