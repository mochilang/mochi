package any2mochi

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"strings"
	"time"
)

// ConvertErlang converts Erlang source code to Mochi using simple regular
// expression parsing. Type information is not extracted.
func ConvertErlang(src string) ([]byte, error) {
	funcs, err := runErlangParser(src)
	if err != nil || len(funcs) == 0 {
		funcs = ParseErlangFuncs(src)
	}
	var out strings.Builder
	for _, fn := range funcs {
		out.WriteString("fun ")
		if fn.Name == "" {
			out.WriteString("fun")
		} else {
			out.WriteString(fn.Name)
		}
		out.WriteByte('(')
		for i, p := range fn.Params {
			if i > 0 {
				out.WriteString(", ")
			}
			out.WriteString(p.Name)
			if p.Type != "" {
				out.WriteString(": ")
				out.WriteString(p.Type)
			}
		}
		out.WriteByte(')')
		if len(fn.Body) == 0 {
			out.WriteString(" {}\n")
		} else {
			out.WriteString(" {\n")
			for _, line := range fn.Body {
				out.WriteString(line)
				out.WriteByte('\n')
			}
			out.WriteString("}\n")
		}
	}
	if out.Len() == 0 {
		return nil, fmt.Errorf("no convertible symbols found\n\nsource snippet:\n%s", numberedSnippet(src))
	}
	return []byte(out.String()), nil
}

// ConvertErlangFile reads the erlang file and converts it to Mochi.
func ConvertErlangFile(path string) ([]byte, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return nil, err
	}
	return ConvertErlang(string(data))
}

// runErlangParser invokes the helper CLI to parse Erlang source to JSON.
func runErlangParser(src string) ([]ErlFunc, error) {
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	cmd := exec.CommandContext(ctx, "any2mochi-erlparse")
	cmd.Stdin = strings.NewReader(src)
	var out bytes.Buffer
	cmd.Stdout = &out
	if err := cmd.Run(); err != nil {
		return nil, err
	}
	var funcs []ErlFunc
	if err := json.Unmarshal(out.Bytes(), &funcs); err != nil {
		return nil, err
	}
	return funcs, nil
}
