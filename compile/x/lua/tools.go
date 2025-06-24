package luacode

import (
	"fmt"
	"os"
	"os/exec"
	"runtime"
)

// EnsureLua verifies that the Lua interpreter is installed. If missing,
// it attempts a best-effort installation on Linux or macOS.
func EnsureLua() error {
	if _, err := exec.LookPath("lua"); err == nil {
		return nil
	}
	switch runtime.GOOS {
	case "linux":
		if _, err := exec.LookPath("apt-get"); err == nil {
			fmt.Println("🔧 Installing Lua...")
			cmd := exec.Command("apt-get", "update")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				return err
			}
			// try to install lua5.4, fallback to lua5.3
			cmd = exec.Command("apt-get", "install", "-y", "lua5.4", "lua-json")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				cmd = exec.Command("apt-get", "install", "-y", "lua5.3", "lua-json")
				cmd.Stdout = os.Stdout
				cmd.Stderr = os.Stderr
				_ = cmd.Run()
			}
		}
	case "darwin":
		if _, err := exec.LookPath("brew"); err == nil {
			fmt.Println("🍺 Installing Lua via Homebrew...")
			cmd := exec.Command("brew", "install", "lua")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	case "windows":
		if _, err := exec.LookPath("choco"); err == nil {
			fmt.Println("🔧 Installing Lua via Chocolatey...")
			cmd := exec.Command("choco", "install", "-y", "lua")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		} else if _, err := exec.LookPath("scoop"); err == nil {
			fmt.Println("🔧 Installing Lua via Scoop...")
			cmd := exec.Command("scoop", "install", "lua")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	}
	if _, err := exec.LookPath("lua"); err == nil {
		return nil
	}
	return fmt.Errorf("lua not found")
}

// EnsureLuaJIT verifies that the LuaJIT interpreter is installed. If missing,
// it attempts a best-effort installation on Linux or macOS.
func EnsureLuaJIT() error {
	if _, err := exec.LookPath("luajit"); err == nil {
		return nil
	}
	switch runtime.GOOS {
	case "linux":
		if _, err := exec.LookPath("apt-get"); err == nil {
			fmt.Println("🔧 Installing LuaJIT...")
			cmd := exec.Command("apt-get", "update")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				return err
			}
			cmd = exec.Command("apt-get", "install", "-y", "luajit", "lua-json")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	case "darwin":
		if _, err := exec.LookPath("brew"); err == nil {
			fmt.Println("🍺 Installing LuaJIT via Homebrew...")
			cmd := exec.Command("brew", "install", "luajit")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	case "windows":
		if _, err := exec.LookPath("choco"); err == nil {
			fmt.Println("🔧 Installing LuaJIT via Chocolatey...")
			cmd := exec.Command("choco", "install", "-y", "luajit")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		} else if _, err := exec.LookPath("scoop"); err == nil {
			fmt.Println("🔧 Installing LuaJIT via Scoop...")
			cmd := exec.Command("scoop", "install", "luajit")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	}
	if _, err := exec.LookPath("luajit"); err == nil {
		return nil
	}
	return fmt.Errorf("luajit not found")
}
