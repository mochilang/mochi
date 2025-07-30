//go:build slow

package rkt

import (
	"bytes"
	_ "embed"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"sort"
	"strings"
)

//go:embed parse.rkt
var racketParser string

// Form represents a top-level form parsed from Racket source.
type Form struct {
	Datum *Datum `json:"datum"`
	Line  int    `json:"line"`
	Col   int    `json:"col"`
}

// Program represents a parsed Racket source file.
type Program struct {
	Forms []Form `json:"forms"`
}

// DatumKind describes the type of value stored in a Datum.
type DatumKind int

const (
	kindSymbol DatumKind = iota
	kindList
	kindString
	kindNumber
	kindBool
	kindNull
)

// Datum represents a parsed Racket datum in a type-safe manner.
type Datum struct {
	Kind DatumKind
	Sym  string
	List []*Datum
	Str  string
	Num  json.Number
	Bool bool
}

func (d Datum) MarshalJSON() ([]byte, error) {
	switch d.Kind {
	case kindSymbol:
		return json.Marshal(map[string]string{"sym": d.Sym})
	case kindList:
		return json.Marshal(d.List)
	case kindString:
		return json.Marshal(d.Str)
	case kindNumber:
		return json.Marshal(d.Num)
	case kindBool:
		return json.Marshal(d.Bool)
	case kindNull:
		return []byte("null"), nil
	default:
		return []byte("null"), nil
	}
}

func (d *Datum) UnmarshalJSON(data []byte) error {
	var v interface{}
	dec := json.NewDecoder(bytes.NewReader(data))
	dec.UseNumber()
	if err := dec.Decode(&v); err != nil {
		return err
	}
	nd, err := datumFromIface(v)
	if err != nil {
		return err
	}
	*d = nd
	return nil
}

func datumFromIface(v interface{}) (Datum, error) {
	switch val := v.(type) {
	case map[string]interface{}:
		if s, ok := val["sym"]; ok && len(val) == 1 {
			return Datum{Kind: kindSymbol, Sym: fmt.Sprint(s)}, nil
		}
		// Treat generic objects as list of key/value pairs.
		keys := make([]string, 0, len(val))
		for k := range val {
			keys = append(keys, k)
		}
		sort.Strings(keys)
		list := make([]*Datum, 0, len(val)*2)
		for _, k := range keys {
			kd := Datum{Kind: kindString, Str: k}
			vd, err := datumFromIface(val[k])
			if err != nil {
				return Datum{}, err
			}
			list = append(list, &kd, &vd)
		}
		return Datum{Kind: kindList, List: list}, nil
	case []interface{}:
		list := make([]*Datum, len(val))
		for i, e := range val {
			child, err := datumFromIface(e)
			if err != nil {
				return Datum{}, err
			}
			list[i] = &child
		}
		return Datum{Kind: kindList, List: list}, nil
	case string:
		return Datum{Kind: kindString, Str: val}, nil
	case json.Number:
		return Datum{Kind: kindNumber, Num: val}, nil
	case bool:
		return Datum{Kind: kindBool, Bool: val}, nil
	case nil:
		return Datum{Kind: kindNull}, nil
	default:
		return Datum{Kind: kindString, Str: fmt.Sprint(val)}, nil
	}
}

// Inspect parses the provided Racket source code and returns a Program
// describing its structure using the official Racket parser.
func Inspect(src string) (*Program, error) {
	prog, err := parse(src)
	if err != nil {
		return nil, err
	}
	return prog, nil
}

func parse(src string) (*Program, error) {
	if _, err := exec.LookPath("racket"); err != nil {
		return nil, fmt.Errorf("racket not installed")
	}
	script, err := os.CreateTemp("", "parse-*.rkt")
	if err != nil {
		return nil, err
	}
	defer os.Remove(script.Name())
	if _, err := script.WriteString(racketParser); err != nil {
		script.Close()
		return nil, err
	}
	script.Close()
	cmd := exec.Command("racket", script.Name())
	cmd.Stdin = strings.NewReader(src)
	var out bytes.Buffer
	var errBuf bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &errBuf
	if err := cmd.Run(); err != nil {
		if errBuf.Len() > 0 {
			return nil, fmt.Errorf("racket: %s", errBuf.String())
		}
		return nil, err
	}
	var rawForms []struct {
		Datum json.RawMessage `json:"datum"`
		Line  int             `json:"line"`
		Col   int             `json:"col"`
	}
	if err := json.Unmarshal(out.Bytes(), &rawForms); err != nil {
		return nil, err
	}
	forms := make([]Form, len(rawForms))
	for i, rf := range rawForms {
		var d Datum
		if err := json.Unmarshal(rf.Datum, &d); err != nil {
			return nil, err
		}
		forms[i] = Form{Datum: &d, Line: rf.Line, Col: rf.Col}
	}
	return &Program{Forms: forms}, nil
}
