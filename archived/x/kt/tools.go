//go:build archived

package ktcode

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"runtime"

	javacode "mochi/archived/x/java"
)

// EnsureKotlin verifies that the Kotlin compiler is installed. If missing,
// it attempts a best-effort installation using Homebrew on macOS or apt-get on Linux.
func EnsureKotlin() error {
	if _, err := exec.LookPath("java"); err != nil {
		switch runtime.GOOS {
		case "darwin":
			if _, err := exec.LookPath("brew"); err == nil {
				cmd := exec.Command("brew", "install", "openjdk")
				cmd.Stdout = os.Stdout
				cmd.Stderr = os.Stderr
				_ = cmd.Run()
			}
		case "linux":
			if _, err := exec.LookPath("apt-get"); err == nil {
				cmd := exec.Command("apt-get", "update")
				cmd.Stdout = os.Stdout
				cmd.Stderr = os.Stderr
				if err := cmd.Run(); err == nil {
					cmd = exec.Command("apt-get", "install", "-y", "openjdk-17-jdk")
					cmd.Stdout = os.Stdout
					cmd.Stderr = os.Stderr
					_ = cmd.Run()
				}
			}
		case "windows":
			if _, err := exec.LookPath("choco"); err == nil {
				cmd := exec.Command("choco", "install", "-y", "openjdk")
				cmd.Stdout = os.Stdout
				cmd.Stderr = os.Stderr
				_ = cmd.Run()
			} else if _, err := exec.LookPath("scoop"); err == nil {
				cmd := exec.Command("scoop", "install", "openjdk")
				cmd.Stdout = os.Stdout
				cmd.Stderr = os.Stderr
				_ = cmd.Run()
			}
		}
		if err := javacode.EnsureJavac(); err != nil {
			return err
		}
	}
	if _, err := exec.LookPath("kotlinc"); err == nil {
		return nil
	}
	switch runtime.GOOS {
	case "darwin":
		if _, err := exec.LookPath("brew"); err == nil {
			cmd := exec.Command("brew", "install", "kotlin")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			return cmd.Run()
		}
		return fmt.Errorf("kotlinc missing; install via Homebrew or Android Studio")
	case "linux":
		if _, err := exec.LookPath("apt-get"); err == nil {
			cmd := exec.Command("apt-get", "update")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				return err
			}
			cmd = exec.Command("apt-get", "install", "-y", "kotlin")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err == nil {
				return nil
			}
		}
		cmd := exec.Command("bash", "-c", "curl -sS https://get.sdkman.io | bash && source $HOME/.sdkman/bin/sdkman-init.sh && sdk install kotlin")
		cmd.Stdout = os.Stdout
		cmd.Stderr = os.Stderr
		return cmd.Run()
	case "windows":
		if _, err := exec.LookPath("choco"); err == nil {
			cmd := exec.Command("choco", "install", "-y", "kotlin")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			return cmd.Run()
		} else if _, err := exec.LookPath("scoop"); err == nil {
			cmd := exec.Command("scoop", "install", "kotlin")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			return cmd.Run()
		}
	}
	return fmt.Errorf("unsupported platform: %s", runtime.GOOS)
}

// EnsureKotlinFormat checks for either ktfmt or ktlint, attempting a best-effort
// installation using common package managers when missing.
func EnsureKotlinFormat() error {
	if _, err := exec.LookPath("ktfmt"); err == nil {
		return nil
	}
	if _, err := exec.LookPath("ktlint"); err == nil {
		return nil
	}
	switch runtime.GOOS {
	case "darwin":
		if _, err := exec.LookPath("brew"); err == nil {
			_ = exec.Command("brew", "install", "ktlint").Run()
		}
	case "linux":
		if _, err := exec.LookPath("apt-get"); err == nil {
			_ = exec.Command("apt-get", "update").Run()
			_ = exec.Command("apt-get", "install", "-y", "ktlint").Run()
		}
	case "windows":
		if _, err := exec.LookPath("choco"); err == nil {
			_ = exec.Command("choco", "install", "-y", "ktlint").Run()
		} else if _, err := exec.LookPath("scoop"); err == nil {
			_ = exec.Command("scoop", "install", "ktlint").Run()
		}
	}
	if _, err := exec.LookPath("ktfmt"); err == nil {
		return nil
	}
	if _, err := exec.LookPath("ktlint"); err == nil {
		return nil
	}
	return fmt.Errorf("ktfmt or ktlint not found")
}

// FormatKotlin runs a formatter on the given source code if available. The
// function first looks for `ktfmt` and then `ktlint`, streaming the input
// through the tool. If neither tool is installed or formatting fails, the
// original input is returned. A trailing newline is always ensured.
func FormatKotlin(src []byte) []byte {
	_ = EnsureKotlinFormat()
	tools := [][]string{{"ktfmt"}, {"ktlint", "-F", "-"}}
	for _, args := range tools {
		if path, err := exec.LookPath(args[0]); err == nil {
			cmd := exec.Command(path, args[1:]...)
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
	}
	src = bytes.ReplaceAll(src, []byte("\t"), []byte("    "))
	lines := bytes.Split(src, []byte{'\n'})
	var buf bytes.Buffer
	indent := 0
	for _, line := range lines {
		trimmed := bytes.TrimSpace(line)
		if len(trimmed) == 0 {
			buf.WriteByte('\n')
			continue
		}
		if trimmed[0] == '}' {
			if indent > 0 {
				indent--
			}
		}
		for i := 0; i < indent; i++ {
			buf.WriteString("    ")
		}
		buf.Write(trimmed)
		buf.WriteByte('\n')
		if trimmed[len(trimmed)-1] == '{' {
			indent++
		}
	}
	res := buf.Bytes()
	if len(res) == 0 || res[len(res)-1] != '\n' {
		res = append(res, '\n')
	}
	return res
}
