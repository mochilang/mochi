package schemecode

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"runtime"
)

// EnsureScheme verifies that chibi-scheme is installed. On Linux it attempts a
// best-effort installation using apt-get, while on macOS it tries Homebrew.
func EnsureScheme() (string, error) {
	if path, err := exec.LookPath("chibi-scheme"); err == nil {
		return path, nil
	}
	switch runtime.GOOS {
	case "darwin":
		if _, err := exec.LookPath("brew"); err == nil {
			cmd := exec.Command("brew", "install", "chibi-scheme")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	case "linux":
		if _, err := exec.LookPath("apt-get"); err == nil {
			cmd := exec.Command("apt-get", "update")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				return "", err
			}
			cmd = exec.Command("apt-get", "install", "-y", "chibi-scheme")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	case "windows":
		if _, err := exec.LookPath("choco"); err == nil {
			cmd := exec.Command("choco", "install", "-y", "chibi-scheme")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		} else if _, err := exec.LookPath("scoop"); err == nil {
			cmd := exec.Command("scoop", "install", "chibi-scheme")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	}
	if path, err := exec.LookPath("chibi-scheme"); err == nil {
		return path, nil
	}
	return "", fmt.Errorf("chibi-scheme not found")
}

// FormatScheme replaces tabs with two spaces and ensures a trailing newline.
// If the chibi-scheme binary is available a best-effort pretty printing is attempted.
// EnsureFormatter verifies that a Scheme formatter is available.
// Currently this just ensures chibi-scheme is installed so its
// built-in pretty printer can be used.
func EnsureFormatter() error {
	if _, err := EnsureScheme(); err == nil {
		return nil
	}
	return fmt.Errorf("chibi-scheme not found")
}

// FormatScheme replaces tabs with two spaces and ensures a trailing newline.
// It attempts to pretty print the code using chibi-scheme when available.
func FormatScheme(src []byte) []byte {
	if err := EnsureFormatter(); err == nil {
		if path, err := exec.LookPath("chibi-scheme"); err == nil {
			cmd := exec.Command(path, "-q", "-e",
				`(import (chibi show pretty))
                                (let loop ((x (read)))
                                  (unless (eof-object? x)
                                    (pretty-print x)
                                    (loop (read))))`)
			cmd.Stdin = bytes.NewReader(src)
			var out bytes.Buffer
			cmd.Stdout = &out
			if e := cmd.Run(); e == nil && out.Len() > 0 {
				src = out.Bytes()
			}
		}
	}
	src = bytes.ReplaceAll(src, []byte("\t"), []byte("  "))
	if len(src) > 0 && src[len(src)-1] != '\n' {
		src = append(src, '\n')
	}
	return src
}
