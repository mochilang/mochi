package main

import (
	"errors"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
)

func main() {
	chibiccPath, chibiccDir, err := resolveChibicc()
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}

	if err := ensureChibicc(chibiccPath, chibiccDir); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}

	cmd := exec.Command(chibiccPath, os.Args[1:]...)
	cmd.Stdin = os.Stdin
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		if exitErr := new(exec.ExitError); errors.As(err, &exitErr) {
			os.Exit(exitErr.ExitCode())
		}
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}

func resolveChibicc() (string, string, error) {
	if override := os.Getenv("CHIBICC"); override != "" {
		path, err := filepath.Abs(override)
		if err != nil {
			return "", "", fmt.Errorf("resolve CHIBICC path: %w", err)
		}
		return path, filepath.Dir(path), nil
	}

	exePath, err := os.Executable()
	if err != nil {
		return "", "", fmt.Errorf("resolve chibigo executable: %w", err)
	}
	baseDir := filepath.Dir(exePath)
	chibiccDir := filepath.Clean(filepath.Join(baseDir, "..", "chibicc"))
	chibiccPath := filepath.Join(chibiccDir, "chibicc")

	return chibiccPath, chibiccDir, nil
}

func ensureChibicc(chibiccPath, chibiccDir string) error {
	if _, err := os.Stat(chibiccPath); err == nil {
		return nil
	}

	makePath, err := exec.LookPath("make")
	if err != nil {
		return fmt.Errorf("chibicc binary not found and make is unavailable: %w", err)
	}

	cmd := exec.Command(makePath)
	cmd.Dir = chibiccDir
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("build chibicc: %w", err)
	}

	if _, err := os.Stat(chibiccPath); err != nil {
		return fmt.Errorf("chibicc build finished but binary missing: %w", err)
	}
	return nil
}
