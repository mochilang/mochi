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
	Name     string   `json:"name"`
	Params   []string `json:"params"`
	Body     []string `json:"body"`
	Line     int      `json:"line"`
	Arity    int      `json:"arity"`
	Exported bool     `json:"exported"`
}

type Record struct {
	Name   string   `json:"name"`
	Fields []string `json:"fields"`
	Line   int      `json:"line"`
}

func parseAST(src string) ([]Func, []Record, error) {
	if _, err := exec.LookPath("escript"); err != nil {
		return nil, nil, fmt.Errorf("escript not found")
	}
	tmp, err := os.CreateTemp("", "src-*.erl")
	if err != nil {
		return nil, nil, err
	}
	defer os.Remove(tmp.Name())
	if _, err := tmp.WriteString(src); err != nil {
		return nil, nil, err
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
			return nil, nil, fmt.Errorf("parse error: %s", perr.Error)
		}
		if stderr.Len() > 0 {
			return nil, nil, fmt.Errorf("%v: %s", err, strings.TrimSpace(stderr.String()))
		}
		return nil, nil, err
	}
	var res struct {
		Functions []Func   `json:"functions"`
		Records   []Record `json:"records"`
	}
	if err := json.Unmarshal(out.Bytes(), &res); err != nil {
		return nil, nil, err
	}
	for i := range res.Functions {
		for j := range res.Functions[i].Body {
			res.Functions[i].Body[j] = strings.ReplaceAll(res.Functions[i].Body[j], "\n", "\\n")
		}
	}
	return res.Functions, res.Records, nil
}
