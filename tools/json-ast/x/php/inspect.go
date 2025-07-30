package php

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"sync"
	"time"
)

// Node represents a php-ast node.
type Node struct {
	Kind     string      `json:"kind"`
	Flags    int         `json:"flags,omitempty"`
	LineNo   int         `json:"lineno,omitempty"`
	Children interface{} `json:"children,omitempty"`
	raw      json.RawMessage
}

func (n *Node) UnmarshalJSON(data []byte) error {
	n.raw = append(n.raw[:0], data...)
	var aux struct {
		Kind     string          `json:"kind"`
		Flags    int             `json:"flags"`
		LineNo   int             `json:"lineno"`
		Children json.RawMessage `json:"children"`
	}
	if err := json.Unmarshal(data, &aux); err != nil {
		return err
	}
	n.Kind = aux.Kind
	n.Flags = aux.Flags
	n.LineNo = aux.LineNo
	if len(aux.Children) == 0 {
		return nil
	}
	switch aux.Children[0] {
	case '{':
		var obj map[string]json.RawMessage
		if err := json.Unmarshal(aux.Children, &obj); err != nil {
			return err
		}
		res := make(map[string]interface{}, len(obj))
		for k, v := range obj {
			if len(v) == 0 {
				res[k] = nil
				continue
			}
			switch v[0] {
			case '{':
				var c Node
				if err := json.Unmarshal(v, &c); err != nil {
					return err
				}
				res[k] = &c
			case '[':
				var arr []json.RawMessage
				if err := json.Unmarshal(v, &arr); err != nil {
					return err
				}
				slice := make([]interface{}, len(arr))
				for i, e := range arr {
					if len(e) == 0 {
						slice[i] = nil
						continue
					}
					if e[0] == '{' {
						var c Node
						if err := json.Unmarshal(e, &c); err == nil {
							slice[i] = &c
							continue
						}
					}
					var val interface{}
					_ = json.Unmarshal(e, &val)
					slice[i] = val
				}
				res[k] = slice
			default:
				var val interface{}
				if err := json.Unmarshal(v, &val); err != nil {
					return err
				}
				res[k] = val
			}
		}
		n.Children = res
	case '[':
		var arr []json.RawMessage
		if err := json.Unmarshal(aux.Children, &arr); err != nil {
			return err
		}
		slice := make([]interface{}, len(arr))
		for i, e := range arr {
			if len(e) == 0 {
				slice[i] = nil
				continue
			}
			if e[0] == '{' {
				var c Node
				if err := json.Unmarshal(e, &c); err == nil {
					slice[i] = &c
					continue
				}
			}
			var val interface{}
			_ = json.Unmarshal(e, &val)
			slice[i] = val
		}
		n.Children = slice
	default:
		n.Children = aux.Children
	}
	return nil
}

func (n Node) MarshalJSON() ([]byte, error) {
	if n.raw != nil {
		return n.raw, nil
	}
	m := map[string]interface{}{
		"kind":   n.Kind,
		"flags":  n.Flags,
		"lineno": n.LineNo,
	}
	if n.Children != nil {
		m["children"] = n.Children
	}
	return json.Marshal(m)
}

// Program represents a parsed PHP file.
type Program struct {
	Root *Node `json:"root"`
}

var (
	extOnce sync.Once
	extErr  error
	script  string
)

func ensureExt() error {
	extOnce.Do(func() {
		// Check if ast extension is available
		cmd := exec.Command("php", "-d", "extension=ast.so", "-r", "exit(0);")
		if err := cmd.Run(); err == nil {
			return
		}
		dir := filepath.Join(os.TempDir(), "php-ast")
		if err := os.RemoveAll(dir); err == nil {
			_ = os.MkdirAll(dir, 0o755)
		}
		if out, err := exec.Command("git", "clone", "--depth=1", "https://github.com/nikic/php-ast.git", dir).CombinedOutput(); err != nil {
			extErr = fmt.Errorf("clone ast: %v: %s", err, out)
			return
		}
		build := func(name string, args ...string) error {
			c := exec.Command(name, args...)
			c.Dir = dir
			if b, err := c.CombinedOutput(); err != nil {
				return fmt.Errorf("%s: %v: %s", name, err, b)
			}
			return nil
		}
		if err := build("phpize"); err != nil {
			extErr = err
			return
		}
		if err := build("./configure"); err != nil {
			extErr = err
			return
		}
		if err := build("make", "-j4"); err != nil {
			extErr = err
			return
		}
		if err := build("make", "install"); err != nil {
			extErr = err
			return
		}
	})
	return extErr
}

func init() {
	if _, file, _, ok := runtime.Caller(0); ok {
		script = filepath.Join(filepath.Dir(file), "dump_ast.php")
	}
}

func Inspect(src string) (*Program, error) {
	if err := ensureExt(); err != nil {
		return nil, fmt.Errorf("ensure ast: %w", err)
	}
	ctx, cancel := context.WithTimeout(context.Background(), 15*time.Second)
	defer cancel()
	cmd := exec.CommandContext(ctx, "php", "-d", "extension=ast.so", script)
	cmd.Stdin = bytes.NewBufferString(src)
	var out bytes.Buffer
	cmd.Stdout = &out
	if err := cmd.Run(); err != nil {
		return nil, err
	}
	var root Node
	if err := json.Unmarshal(out.Bytes(), &root); err != nil {
		return nil, err
	}
	return &Program{Root: &root}, nil
}
