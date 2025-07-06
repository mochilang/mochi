package erlang

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"time"
)

type Func struct {
	Name   string   `json:"name"`
	Params []string `json:"params"`
	Body   []string `json:"body"`
	Line   int      `json:"line"`
	Arity  int      `json:"arity"`
}

func parseAST(src string) ([]Func, error) {
	tmp, err := os.CreateTemp("", "src-*.erl")
	if err != nil {
		return nil, err
	}
	defer os.Remove(tmp.Name())
	if _, err := tmp.WriteString(src); err != nil {
		return nil, err
	}
	tmp.Close()
	script := filepath.Join("tools", "any2mochi", "x", "erlang", "parser", "parser.escript")
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	cmd := exec.CommandContext(ctx, "escript", script, tmp.Name())
	var out bytes.Buffer
	var stderr bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		var perr struct {
			Error string `json:"error"`
		}
		if jsonErr := json.Unmarshal(out.Bytes(), &perr); jsonErr == nil && perr.Error != "" {
			return nil, fmt.Errorf("parse error: %s", perr.Error)
		}
		if stderr.Len() > 0 {
			return nil, fmt.Errorf("%v: %s", err, strings.TrimSpace(stderr.String()))
		}
		return nil, err
	}
	var res struct {
		Functions []Func `json:"functions"`
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
