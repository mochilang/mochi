package chibigo

import (
	"errors"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
)

type Compiler struct {
	Binary   string
	WorkDir  string
	ExtraEnv []string
}

func New(binary string) (*Compiler, error) {
	if binary == "" {
		return nil, errors.New("binary path is required")
	}
	abs, err := filepath.Abs(binary)
	if err != nil {
		return nil, fmt.Errorf("resolve binary path: %w", err)
	}
	if _, err := os.Stat(abs); err != nil {
		return nil, fmt.Errorf("chibicc binary not found: %w", err)
	}
	return &Compiler{Binary: abs}, nil
}

func (c *Compiler) Compile(args ...string) error {
	if c == nil {
		return errors.New("compiler is nil")
	}
	if c.Binary == "" {
		return errors.New("compiler binary is empty")
	}
	cmd := exec.Command(c.Binary, args...)
	if c.WorkDir != "" {
		cmd.Dir = c.WorkDir
	}
	cmd.Env = append(os.Environ(), c.ExtraEnv...)
	cmd.Stdout = os.Stdout
	cmd.Stderr = os.Stderr
	if err := cmd.Run(); err != nil {
		return fmt.Errorf("run chibicc: %w", err)
	}
	return nil
}
