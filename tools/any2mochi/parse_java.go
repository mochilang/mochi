package any2mochi

import (
	"bytes"
	"context"
	"encoding/json"
	"os/exec"
	"time"
)

// parseJava invokes the mochi-javaast CLI to convert Java source to Mochi statements.
func parseJava(src string) ([]string, error) {
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	cmd := exec.CommandContext(ctx, "go", "run", "mochi/cmd/javaast")
	cmd.Stdin = bytes.NewBufferString(src)
	var out bytes.Buffer
	cmd.Stdout = &out
	if err := cmd.Run(); err != nil {
		return nil, err
	}
	var res struct {
		Lines []string `json:"lines"`
	}
	if err := json.Unmarshal(out.Bytes(), &res); err != nil {
		return nil, err
	}
	return res.Lines, nil
}
