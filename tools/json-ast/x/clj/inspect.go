package clj

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"os/exec"
	"strings"
	"time"
)

// Node represents a Clojure form in a type-safe structure.
type Node struct {
	Sym    string     `json:"sym,omitempty"`
	Kw     string     `json:"kw,omitempty"`
	Str    string     `json:"str,omitempty"`
	Num    *float64   `json:"num,omitempty"`
	Bool   *bool      `json:"bool,omitempty"`
	Nil    bool       `json:"nil,omitempty"`
	List   []*Node    `json:"list,omitempty"`
	Vector []*Node    `json:"vector,omitempty"`
	Map    []MapEntry `json:"map,omitempty"`
}

// MapEntry represents a key/value pair within a map literal.
type MapEntry struct {
	Key *Node `json:"key"`
	Val *Node `json:"val"`
}

// Program represents a parsed Clojure source file.
type Program struct {
	Forms []*Node `json:"forms"`
}

const bbScript = `(require '[cheshire.core :as json])

(defn node->json [x]
  (cond
    (list? x) {:list (mapv node->json x)}
    (vector? x) {:vector (mapv node->json x)}
    (map? x) {:map (mapv (fn [[k v]] {:key (node->json k) :val (node->json v)}) x)}
    (symbol? x) {:sym (str x)}
    (keyword? x) {:kw (str x)}
    (string? x) {:str x}
    (number? x) {:num x}
    (boolean? x) {:bool x}
    (nil? x) {:nil true}
    :else {:unknown (pr-str x)}))

(let [forms (read-string (str "[" (slurp *in*) "]"))]
  (println (json/generate-string (mapv node->json forms))))`

var babashkaCmd = "bb"

// Inspect parses the given Clojure source code and returns a Program describing its forms.
func Inspect(src string) (*Program, error) {
	if err := EnsureBabashka(); err != nil {
		return nil, err
	}
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	cmd := exec.CommandContext(ctx, babashkaCmd, "-e", bbScript)
	cmd.Stdin = strings.NewReader(src)
	var out, errBuf bytes.Buffer
	cmd.Stdout = &out
	cmd.Stderr = &errBuf
	if err := cmd.Run(); err != nil {
		msg := strings.TrimSpace(errBuf.String())
		if msg != "" {
			return nil, fmt.Errorf("%v: %s", err, msg)
		}
		return nil, err
	}
	var forms []*Node
	if err := json.Unmarshal(out.Bytes(), &forms); err != nil {
		return nil, err
	}
	return &Program{Forms: forms}, nil
}
