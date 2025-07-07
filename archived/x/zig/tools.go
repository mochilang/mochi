//go:build archived

package zigcode

import (
	"bufio"
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
)

// EnsureZig verifies that the Zig compiler is installed.
func EnsureZig() (string, error) {
	if path, err := exec.LookPath("zig"); err == nil {
		return path, nil
	}

	dir := filepath.Join(os.TempDir(), "zig-0.12.0")
	bin := filepath.Join(dir, "zig")
	if _, err := os.Stat(bin); err == nil {
		return bin, nil
	}

	fmt.Println("\U0001F98E Installing Zig 0.12.0...")
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return "", err
	}

	var url string
	switch runtime.GOOS {
	case "linux":
		if runtime.GOARCH == "arm64" || runtime.GOARCH == "aarch64" {
			url = "https://ziglang.org/download/0.12.0/zig-linux-aarch64-0.12.0.tar.xz"
		} else {
			url = "https://ziglang.org/download/0.12.0/zig-linux-x86_64-0.12.0.tar.xz"
		}
	case "darwin":
		if runtime.GOARCH == "arm64" {
			url = "https://ziglang.org/download/0.12.0/zig-macos-aarch64-0.12.0.tar.xz"
		} else {
			url = "https://ziglang.org/download/0.12.0/zig-macos-x86_64-0.12.0.tar.xz"
		}
	case "windows":
		if runtime.GOARCH == "arm64" || runtime.GOARCH == "aarch64" {
			url = "https://ziglang.org/download/0.12.0/zig-windows-aarch64-0.12.0.zip"
		} else {
			url = "https://ziglang.org/download/0.12.0/zig-windows-x86_64-0.12.0.zip"
		}
	default:
		return "", fmt.Errorf("unsupported platform: %s/%s", runtime.GOOS, runtime.GOARCH)
	}

	tarPath := filepath.Join(dir, "zig.tar.xz")
	if runtime.GOOS == "windows" {
		tarPath = filepath.Join(dir, "zig.zip")
	}
	if out, err := exec.Command("curl", "-fsSL", "-o", tarPath, url).CombinedOutput(); err != nil {
		return "", fmt.Errorf("download zig: %v\n%s", err, out)
	}
	if runtime.GOOS == "windows" {
		if out, err := exec.Command("unzip", "-q", tarPath, "-d", dir).CombinedOutput(); err != nil {
			return "", fmt.Errorf("extract zig: %v\n%s", err, out)
		}
	} else {
		if out, err := exec.Command("tar", "-C", dir, "--strip-components=1", "-xf", tarPath).CombinedOutput(); err != nil {
			return "", fmt.Errorf("extract zig: %v\n%s", err, out)
		}
	}
	if _, err := os.Stat(bin); err != nil {
		return "", fmt.Errorf("zig not installed")
	}
	return bin, nil
}

// EnsureFormatter ensures the zig formatter is available. It attempts to
// install Zig if the binary cannot be found.
func EnsureFormatter() error {
	if _, err := exec.LookPath("zig"); err == nil {
		return nil
	}
	_, err := EnsureZig()
	return err
}

// Format runs `zig fmt` on the provided source code. If the Zig compiler
// isn't available, the input is returned unchanged.
func Format(src []byte) []byte {
	if err := EnsureFormatter(); err == nil {
		path, _ := exec.LookPath("zig")
		cmd := exec.Command(path, "fmt", "--stdin")
		cmd.Stdin = bytes.NewReader(src)
		var out bytes.Buffer
		cmd.Stdout = &out
		if err := cmd.Run(); err == nil {
			res := out.Bytes()
			if len(res) == 0 || res[len(res)-1] != '\n' {
				res = append(res, '\n')
			}
			return res
		}
	}
	// fallback: convert tabs to spaces and trim trailing whitespace
	s := strings.ReplaceAll(string(src), "\t", "    ")
	var buf bytes.Buffer
	scanner := bufio.NewScanner(strings.NewReader(s))
	for scanner.Scan() {
		line := strings.TrimRight(scanner.Text(), " \t")
		buf.WriteString(line)
		buf.WriteByte('\n')
	}
	res := buf.Bytes()
	if len(res) == 0 || res[len(res)-1] != '\n' {
		res = append(res, '\n')
	}
	return res
}
