//go:build slow

package clj

import (
	"bytes"
	_ "embed"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"strings"
)

//go:embed parse.clj
var parseCLJ string

// Program represents the top-level forms returned by parse.clj.
type Program struct {
	Forms  []form `json:"forms"`
	Source string `json:"-"`
}

type form struct {
	Type    string   `json:"type"`
	Name    string   `json:"name,omitempty"`
	Params  []string `json:"params,omitempty"`
	Doc     string   `json:"doc,omitempty"`
	Body    []node   `json:"body,omitempty"`
	Value   node     `json:"value,omitempty"`
	Line    int      `json:"line,omitempty"`
	Col     int      `json:"col,omitempty"`
	EndLine int      `json:"end-line,omitempty"`
	EndCol  int      `json:"end-col,omitempty"`
}

type node struct {
	Atom    string `json:"atom,omitempty"`
	List    []node `json:"list,omitempty"`
	Line    int    `json:"line,omitempty"`
	Col     int    `json:"col,omitempty"`
	EndLine int    `json:"end-line,omitempty"`
	EndCol  int    `json:"end-col,omitempty"`
}

// Parse reads Clojure source code using the parse.clj helper script.
func Parse(src string) (*Program, error) {
	tmp, err := os.CreateTemp("", "clj-src-*.clj")
	if err != nil {
		return nil, err
	}
	if _, err := tmp.WriteString(src); err != nil {
		tmp.Close()
		os.Remove(tmp.Name())
		return nil, err
	}
	tmp.Close()
	defer os.Remove(tmp.Name())

	scriptFile, err := os.CreateTemp("", "parse-*.clj")
	if err != nil {
		return nil, err
	}
	if _, err := scriptFile.WriteString(parseCLJ); err != nil {
		scriptFile.Close()
		os.Remove(scriptFile.Name())
		return nil, err
	}
	scriptFile.Close()
	defer os.Remove(scriptFile.Name())

	cmd := exec.Command(
		"clojure",
		"-Sdeps",
		"{:deps {org.clojure/data.json {:mvn/version \"2.5.0\"}}}",
		"-M",
		scriptFile.Name(),
		tmp.Name(),
	)
	var out bytes.Buffer
	var stderr bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &stderr
	if err := cmd.Run(); err != nil {
		msg := strings.TrimSpace(stderr.String())
		if msg == "" {
			msg = err.Error()
		}
		return nil, fmt.Errorf("clojure: %s", msg)
	}
	var prog Program
	if err := json.Unmarshal(out.Bytes(), &prog); err != nil {
		return nil, err
	}
	prog.Source = src
	return &prog, nil
}
