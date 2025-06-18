package zigcode

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
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
	default:
		return "", fmt.Errorf("unsupported platform: %s/%s", runtime.GOOS, runtime.GOARCH)
	}

	tarPath := filepath.Join(dir, "zig.tar.xz")
	if out, err := exec.Command("curl", "-fsSL", "-o", tarPath, url).CombinedOutput(); err != nil {
		return "", fmt.Errorf("download zig: %v\n%s", err, out)
	}
	if out, err := exec.Command("tar", "-C", dir, "--strip-components=1", "-xf", tarPath).CombinedOutput(); err != nil {
		return "", fmt.Errorf("extract zig: %v\n%s", err, out)
	}
	if _, err := os.Stat(bin); err != nil {
		return "", fmt.Errorf("zig not installed")
	}
	return bin, nil
}
