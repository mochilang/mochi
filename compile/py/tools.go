package pycode

import (
	"fmt"
	"os"
	"os/exec"
	"runtime"
)

// EnsurePython installs Python3 if missing. Useful for benchmarks and tests.
func EnsurePython() error {
	if _, err := exec.LookPath("python3"); err == nil {
		return nil
	}
	switch runtime.GOOS {
	case "darwin":
		if _, err := exec.LookPath("brew"); err == nil {
			fmt.Println("\U0001F40D Installing Python3 via Homebrew...")
			cmd := exec.Command("brew", "install", "python")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	case "linux":
		if _, err := exec.LookPath("apt-get"); err == nil {
			fmt.Println("\U0001F40D Installing Python3...")
			cmd := exec.Command("apt-get", "update")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				return err
			}
			cmd = exec.Command("apt-get", "install", "-y", "python3")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	case "windows":
		if _, err := exec.LookPath("choco"); err == nil {
			fmt.Println("🐍 Installing Python via Chocolatey...")
			cmd := exec.Command("choco", "install", "-y", "python")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		} else if _, err := exec.LookPath("scoop"); err == nil {
			fmt.Println("🐍 Installing Python via Scoop...")
			cmd := exec.Command("scoop", "install", "python")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	}
	if _, err := exec.LookPath("python3"); err == nil {
		return nil
	}
	return fmt.Errorf("python3 not found")
}

// EnsurePyPy installs pypy3 if missing.
func EnsurePyPy() error {
	if _, err := exec.LookPath("pypy3"); err == nil {
		return nil
	}
	switch runtime.GOOS {
	case "darwin":
		if _, err := exec.LookPath("brew"); err == nil {
			fmt.Println("🐍 Installing PyPy3 via Homebrew...")
			cmd := exec.Command("brew", "install", "pypy3")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	case "linux":
		if _, err := exec.LookPath("apt-get"); err == nil {
			fmt.Println("🐍 Installing PyPy3...")
			cmd := exec.Command("apt-get", "update")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				return err
			}
			cmd = exec.Command("apt-get", "install", "-y", "pypy3")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	case "windows":
		if _, err := exec.LookPath("choco"); err == nil {
			fmt.Println("🐍 Installing PyPy3 via Chocolatey...")
			cmd := exec.Command("choco", "install", "-y", "pypy3")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		} else if _, err := exec.LookPath("scoop"); err == nil {
			fmt.Println("🐍 Installing PyPy3 via Scoop...")
			cmd := exec.Command("scoop", "install", "pypy")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	}
	if _, err := exec.LookPath("pypy3"); err == nil {
		return nil
	}
	return fmt.Errorf("pypy3 not found")
}

// EnsureCython installs Cython if missing.
func EnsureCython() error {
	if _, err := exec.LookPath("cython"); err == nil {
		return nil
	}
	switch runtime.GOOS {
	case "linux":
		if _, err := exec.LookPath("apt-get"); err == nil {
			fmt.Println("🐍 Installing Cython...")
			cmd := exec.Command("apt-get", "update")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				return err
			}
			cmd = exec.Command("apt-get", "install", "-y", "cython3")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	case "darwin":
		if _, err := exec.LookPath("brew"); err == nil {
			fmt.Println("🐍 Installing Cython via Homebrew...")
			cmd := exec.Command("brew", "install", "cython")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	case "windows":
		if _, err := exec.LookPath("choco"); err == nil {
			fmt.Println("🐍 Installing Cython via Chocolatey...")
			cmd := exec.Command("choco", "install", "-y", "cython")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		} else if _, err := exec.LookPath("scoop"); err == nil {
			fmt.Println("🐍 Installing Cython via Scoop...")
			cmd := exec.Command("scoop", "install", "cython")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	default:
		if pip, err := exec.LookPath("pip3"); err == nil {
			fmt.Println("🐍 Installing Cython via pip...")
			cmd := exec.Command(pip, "install", "--user", "cython")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	}
	if _, err := exec.LookPath("cython"); err == nil {
		return nil
	}
	return fmt.Errorf("cython not found")
}
