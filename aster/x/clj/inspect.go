package clj

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"os/exec"
	"strconv"
	"strings"
	"time"
)

// Program represents a parsed Clojure source file.
type Program struct {
	Forms []*Form `json:"forms"`
}

// Option controls how Inspect behaves.
// Currently no options are honored but the structure mirrors other
// language packages and allows future extensions.
type Option struct {
	// Positions enables positional information in the resulting AST.
	// Clojure inspection via babashka currently does not provide
	// positions so this field is ignored.
	Positions bool
}

// rawNode mirrors the JSON structure produced by the babashka script.
// It is converted into the generic Node type by toNode.
type rawNode struct {
	Sym    string     `json:"sym,omitempty"`
	Kw     string     `json:"kw,omitempty"`
	Str    string     `json:"str,omitempty"`
	Num    *float64   `json:"num,omitempty"`
	Bool   *bool      `json:"bool,omitempty"`
	Nil    bool       `json:"nil,omitempty"`
	List   []*rawNode `json:"list,omitempty"`
	Vector []*rawNode `json:"vector,omitempty"`
	Map    []rawEntry `json:"map,omitempty"`
}

type rawEntry struct {
	Key *rawNode `json:"key"`
	Val *rawNode `json:"val"`
}

// toNode converts a rawNode into the generic Node representation.
func toNode(r *rawNode) *Node {
	if r == nil {
		return nil
	}
	switch {
	case r.Sym != "":
		return &Node{Kind: "symbol", Text: r.Sym}
	case r.Kw != "":
		return &Node{Kind: "keyword", Text: r.Kw}
	case r.Str != "":
		return &Node{Kind: "string", Text: r.Str}
	case r.Num != nil:
		return &Node{Kind: "number", Text: strconv.FormatFloat(*r.Num, 'f', -1, 64)}
	case r.Bool != nil:
		if *r.Bool {
			return &Node{Kind: "boolean", Text: "true"}
		}
		return &Node{Kind: "boolean", Text: "false"}
	case r.Nil:
		return &Node{Kind: "nil", Text: "nil"}
	case len(r.List) > 0:
		n := &Node{Kind: "list"}
		for _, c := range r.List {
			if cn := toNode(c); cn != nil {
				n.Children = append(n.Children, cn)
			}
		}
		return n
	case len(r.Vector) > 0:
		n := &Node{Kind: "vector"}
		for _, c := range r.Vector {
			if cn := toNode(c); cn != nil {
				n.Children = append(n.Children, cn)
			}
		}
		return n
	case len(r.Map) > 0:
		n := &Node{Kind: "map"}
		for _, e := range r.Map {
			en := &Node{Kind: "entry"}
			en.Children = []*Node{toNode(e.Key), toNode(e.Val)}
			n.Children = append(n.Children, en)
		}
		return n
	}
	return nil
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
// Inspect parses the given Clojure source code. The returned Program uses the
// generic Node structure defined in ast.go. Position information is not
// currently provided.
func Inspect(src string) (*Program, error) {
	return InspectWithOption(src, Option{})
}

// InspectWithOption behaves like Inspect but allows callers to specify whether
// additional information should be captured. The returned AST structure is the
// same regardless of the options provided.
func InspectWithOption(src string, opt Option) (*Program, error) {
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
	var rawForms []*rawNode
	if err := json.Unmarshal(out.Bytes(), &rawForms); err != nil {
		return nil, err
	}
	var forms []*Form
	for _, rf := range rawForms {
		if n := toNode(rf); n != nil {
			forms = append(forms, (*Form)(n))
		}
	}
	return &Program{Forms: forms}, nil
}
