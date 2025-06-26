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
		return ensureLuaJSON()
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
			cmd = exec.Command("apt-get", "install", "-y", "lua5.4")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				cmd = exec.Command("apt-get", "install", "-y", "lua5.3")
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
		return ensureLuaJSON()
	}
	return fmt.Errorf("lua not found")
}

func ensureLuaJSON() error {
	script := "require('json')"
	cmd := exec.Command("lua", "-e", script)
	if err := cmd.Run(); err == nil {
		return nil
	}
	script = "require('cjson')"
	cmd = exec.Command("lua", "-e", script)
	if err := cmd.Run(); err == nil {
		return nil
	}
	switch runtime.GOOS {
	case "linux":
		if _, err := exec.LookPath("apt-get"); err == nil {
			fmt.Println("🔧 Installing Lua JSON library...")
			cmd := exec.Command("apt-get", "install", "-y", "lua-cjson")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			if err := cmd.Run(); err != nil {
				cmd = exec.Command("apt-get", "install", "-y", "lua-json")
				cmd.Stdout = os.Stdout
				cmd.Stderr = os.Stderr
				_ = cmd.Run()
			}
		}
	case "darwin":
		if _, err := exec.LookPath("brew"); err == nil {
			fmt.Println("🍺 Installing Lua JSON library via Homebrew...")
			cmd := exec.Command("brew", "install", "lua-cjson")
			cmd.Stdout = os.Stdout
			cmd.Stderr = os.Stderr
			_ = cmd.Run()
		}
	}
	// check again
	cmd = exec.Command("lua", "-e", "require('json')")
	if err := cmd.Run(); err == nil {
		return nil
	}
	cmd = exec.Command("lua", "-e", "require('cjson')")
	if err := cmd.Run(); err == nil {
		return nil
	}
	return fmt.Errorf("lua json library not found")
}
