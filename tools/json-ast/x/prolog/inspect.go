package prolog

import (
	"bytes"
	_ "embed"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"strings"
)

//go:embed pl_ast.pl
var plScript string

// Program represents a parsed Prolog source file.
type Program struct {
	Clauses []Clause `json:"clauses"`
}

// Term holds a generic Prolog term in raw JSON form.
// Term represents a Prolog term.  It tries to decode the JSON produced by the
// SWI-Prolog script into a more structured form.
type Term struct {
	// For variables {"var":"Name"}
	Var string `json:"var,omitempty"`

	// For compound terms {"functor":"=", "args":[...]}
	Functor string `json:"functor,omitempty"`
	Args    []Term `json:"args,omitempty"`

	// For lists [a,b,c]
	List []Term `json:"-"`

	// For atoms or strings "foo"
	Atom string `json:"-"`

	// For numbers 1, 1.5 ...
	Number *float64 `json:"-"`

	// For booleans true/false
	Bool *bool `json:"-"`
}

// UnmarshalJSON decodes the various JSON forms of a term.
func (t *Term) UnmarshalJSON(b []byte) error {
	if len(b) == 0 {
		return nil
	}
	// clear fields
	t.Var = ""
	t.Functor = ""
	t.Args = nil
	t.List = nil
	t.Atom = ""
	t.Number = nil
	t.Bool = nil

	switch b[0] {
	case '{':
		var obj map[string]json.RawMessage
		if err := json.Unmarshal(b, &obj); err != nil {
			return err
		}
		if v, ok := obj["var"]; ok {
			if err := json.Unmarshal(v, &t.Var); err != nil {
				return err
			}
			return nil
		}
		if f, ok := obj["functor"]; ok {
			if err := json.Unmarshal(f, &t.Functor); err != nil {
				return err
			}
			if a, ok := obj["args"]; ok {
				if err := json.Unmarshal(a, &t.Args); err != nil {
					return err
				}
			}
			return nil
		}
		// Treat as a dict object
		for k, v := range obj {
			var child Term
			if err := json.Unmarshal(v, &child); err != nil {
				return err
			}
			t.Functor = "{}"
			t.Args = append(t.Args, Term{Functor: ":", Args: []Term{{Atom: k}, child}})
		}
		return nil
	case '[':
		return json.Unmarshal(b, &t.List)
	case '"':
		return json.Unmarshal(b, &t.Atom)
	case 't', 'f':
		// boolean literal
		var v bool
		if err := json.Unmarshal(b, &v); err != nil {
			return err
		}
		t.Bool = &v
		return nil
	default:
		var num float64
		if err := json.Unmarshal(b, &num); err == nil {
			t.Number = &num
			return nil
		}
	}
	return fmt.Errorf("unknown term: %s", string(b))
}

// MarshalJSON encodes the term back to the JSON format understood by the
// golden files.
func (t Term) MarshalJSON() ([]byte, error) {
	switch {
	case t.Var != "":
		return json.Marshal(struct {
			Var string `json:"var"`
		}{t.Var})
	case t.Functor != "":
		return json.Marshal(struct {
			Args    []Term `json:"args,omitempty"`
			Functor string `json:"functor"`
		}{t.Args, t.Functor})
	case t.List != nil:
		return json.Marshal(t.List)
	case t.Number != nil:
		return json.Marshal(*t.Number)
	case t.Bool != nil:
		return json.Marshal(*t.Bool)
	default:
		return json.Marshal(t.Atom)
	}
}

// Clause describes a single predicate clause.
type Clause struct {
	Name   string `json:"name"`
	Params []Term `json:"params"`
	Goal   Term   `json:"goal"`
	Start  int    `json:"start"`
	End    int    `json:"end"`
}

// Inspect parses the given Prolog source code using swipl and returns a Program describing its AST.
func Inspect(src string) (*Program, error) {
	exe := os.Getenv("SWIPL")
	if exe == "" {
		exe = "swipl"
	}
	if _, err := exec.LookPath(exe); err != nil {
		return nil, fmt.Errorf("%s not installed", exe)
	}

	tmp, err := os.CreateTemp("", "pl-*.pl")
	if err != nil {
		return nil, err
	}
	if _, err := tmp.WriteString(plScript); err != nil {
		tmp.Close()
		os.Remove(tmp.Name())
		return nil, err
	}
	tmp.Close()
	defer os.Remove(tmp.Name())

	cmd := exec.Command(exe, "-q", "-f", tmp.Name(), "-t", "main")
	cmd.Stdin = strings.NewReader(src)
	var out, errBuf bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &errBuf
	if err := cmd.Run(); err != nil {
		if errBuf.Len() > 0 {
			return nil, fmt.Errorf("%s", strings.TrimSpace(errBuf.String()))
		}
		return nil, err
	}

	var prog Program
	if err := json.Unmarshal(out.Bytes(), &prog); err != nil {
		return nil, err
	}
	return &prog, nil
}
