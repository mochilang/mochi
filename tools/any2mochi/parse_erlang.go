package any2mochi

import (
	"bytes"
	"context"
	"encoding/json"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"time"
)

type erlFunc struct {
	Name   string   `json:"name"`
	Params []string `json:"params"`
	Body   []string `json:"body"`
}

func parseErlangAST(src string) ([]erlFunc, error) {
	tmp, err := os.CreateTemp("", "src-*.erl")
	if err != nil {
		return nil, err
	}
	defer os.Remove(tmp.Name())
	if _, err := tmp.WriteString(src); err != nil {
		return nil, err
	}
	tmp.Close()
	script := filepath.Join("tools", "any2mochi", "erlang_parser", "parser.escript")
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	cmd := exec.CommandContext(ctx, "escript", script, tmp.Name())
	var out bytes.Buffer
	cmd.Stdout = &out
	if err := cmd.Run(); err != nil {
		return nil, err
	}
	var res struct {
		Functions []erlFunc `json:"functions"`
	}
	if err := json.Unmarshal(out.Bytes(), &res); err != nil {
		return nil, err
	}
	for i := range res.Functions {
		for j := range res.Functions[i].Body {
			res.Functions[i].Body[j] = strings.ReplaceAll(res.Functions[i].Body[j], "\n", "\\n")
		}
	}
	return res.Functions, nil
}
