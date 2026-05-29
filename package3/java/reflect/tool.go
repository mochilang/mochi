package reflect

import (
	"bytes"
	"context"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"

	_ "embed"
)

//go:embed java/reflect-tool.jar
var reflectToolJAR []byte

// Executor is the function type used to run commands. The default uses
// exec.CommandContext; tests inject a mock that returns fixture JSON.
type Executor func(ctx context.Context, name string, args ...string) ([]byte, error)

// defaultExecutor runs the named command and returns combined stdout.
func defaultExecutor(ctx context.Context, name string, args ...string) ([]byte, error) {
	cmd := exec.CommandContext(ctx, name, args...)
	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		return nil, fmt.Errorf("reflect: %s: %w\nstderr: %s", name, err, stderr.String())
	}
	return stdout.Bytes(), nil
}

// Tool invokes the bundled reflect-tool.jar against a target JAR.
type Tool struct {
	// exec overrides the executor for tests. nil uses defaultExecutor.
	exec Executor
}

// NewTool returns a Tool with the default executor.
func NewTool() *Tool { return &Tool{} }

// NewToolWithExecutor returns a Tool with a custom executor (for tests).
func NewToolWithExecutor(ex Executor) *Tool { return &Tool{exec: ex} }

// Invoke runs the reflect tool against jarPath and returns the parsed Surface.
// It extracts the embedded JAR to a temporary file, runs it via java, and
// parses the JSON output.
func (t *Tool) Invoke(ctx context.Context, jarPath string) (*Surface, error) {
	// Extract the embedded tool JAR to a temp file.
	tmpDir, err := os.MkdirTemp("", "mochi-reflect-")
	if err != nil {
		return nil, fmt.Errorf("reflect: create temp dir: %w", err)
	}
	defer os.RemoveAll(tmpDir)

	toolPath := filepath.Join(tmpDir, "reflect-tool.jar")
	if err := os.WriteFile(toolPath, reflectToolJAR, 0o644); err != nil {
		return nil, fmt.Errorf("reflect: write tool jar: %w", err)
	}

	ex := t.exec
	if ex == nil {
		ex = defaultExecutor
	}

	out, err := ex(ctx, "java", "-jar", toolPath, jarPath)
	if err != nil {
		return nil, fmt.Errorf("reflect: invoke tool: %w", err)
	}
	return ParseSurface(out)
}
