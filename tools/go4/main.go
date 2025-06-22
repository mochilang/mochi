package main

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
)

func compileWithGCC(src, out string, asm bool) error {
	var cmd *exec.Cmd
	if asm {
		cmd = exec.Command("gcc", "-w", "-S", src, "-o", out)
	} else {
		cmd = exec.Command("gcc", "-w", "-o", out, src)
	}
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	return cmd.Run()
}

func runBinary(path string, args ...string) error {
	cmd := exec.Command(path, args...)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	return cmd.Run()
}

func usage() {
	fmt.Println("usage: go4 [-s] file [file ...]")
	os.Exit(1)
}

func main() {
	args := os.Args[1:]
	showAsm := false
	if len(args) > 0 && args[0] == "-s" {
		showAsm = true
		args = args[1:]
	}
	if len(args) == 0 {
		usage()
	}

	tmpDir, err := os.MkdirTemp("", "go4-")
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	defer os.RemoveAll(tmpDir)

	exe := filepath.Join(tmpDir, "prog0")
	if err := compileWithGCC(args[0], exe, showAsm); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
	if len(args) == 1 {
		if !showAsm {
			if err := runBinary(exe); err != nil {
				fmt.Fprintln(os.Stderr, err)
				os.Exit(1)
			}
		}
		return
	}

	prev := exe
	for i, src := range args[1:] {
		if err := runBinary(prev, src); err != nil {
			fmt.Fprintln(os.Stderr, err)
			os.Exit(1)
		}
		if i < len(args)-2 {
			next := filepath.Join(tmpDir, fmt.Sprintf("prog%d", i+1))
			if err := os.Rename("a.out", next); err != nil {
				fmt.Fprintln(os.Stderr, err)
				os.Exit(1)
			}
			prev = next
		}
	}
}
