package php

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"sync"
	"time"
)

// Program represents a parsed PHP file.
type Program struct {
	Root json.RawMessage `json:"root"`
}

// Inspect parses the given PHP source code using the official php-parser
// library and returns a Program describing its AST.
var (
	parserOnce sync.Once
	parserPath string
	parserErr  error
)

func ensureParser() error {
	parserOnce.Do(func() {
		dir := filepath.Join(os.TempDir(), "php-parser")
		parserPath = filepath.Join(dir, "bin", "php-parse")
		if _, err := os.Stat(parserPath); err == nil {
			return
		}
		cmd := exec.Command("composer", "create-project", "--no-interaction", "--quiet", "nikic/php-parser", dir)
		parserErr = cmd.Run()
	})
	return parserErr
}

func Inspect(src string) (*Program, error) {
	if err := ensureParser(); err != nil {
		return nil, fmt.Errorf("install parser: %w", err)
	}
	ctx, cancel := context.WithTimeout(context.Background(), 15*time.Second)
	defer cancel()
	cmd := exec.CommandContext(ctx, "php", parserPath, "--json-dump", "-")
	cmd.Stdin = bytes.NewBufferString(src)
	var out bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = nil
	if err := cmd.Run(); err != nil {
		return nil, err
	}
	return &Program{Root: json.RawMessage(out.Bytes())}, nil
}
