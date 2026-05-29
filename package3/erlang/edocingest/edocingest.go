// Package edocingest is the EDoc XML fallback ingest layer for the MEP-66
// Erlang bridge. It is invoked when a package ships neither a Dbgi chunk
// nor an Abst chunk — which happens for packages like lager, observer_cli,
// and cuttlefish that were compiled without debug_info.
//
// EDoc XML format:
//
// When rebar3 generates EDoc documentation, it produces one XML file per
// module under doc/edoc/. The root element is <module> or <edoc>; function
// documentation is under <functions><function>...</function></functions>.
// Each <function> element may carry <typespec> children whose text content
// is the Erlang -spec string (in Erlang term syntax, not XML).
//
// This ingest layer extracts:
//  1. The module name from the <module name="..."> attribute.
//  2. Exported function signatures from <function name="..." arity="...">.
//  3. Type information from <typespec> children (string form only, as EDoc
//     does not emit structured type trees — callers pass these to typemap
//     for best-effort parsing).
//
// Because EDoc XML is an unofficial format and varies across rebar3/EDoc
// versions, all extracted information is marked with SkipEDoc reason when
// it reaches the type-mapping layer (phase 4). The ingest layer itself only
// extracts strings; it does not attempt Erlang type-term parsing.
package edocingest

import (
	"encoding/xml"
	"fmt"
	"io"
	"strings"
)

// FunctionDoc holds the EDoc-extracted documentation for a single exported
// function. The type information is raw string form (Erlang syntax).
type FunctionDoc struct {
	// Name is the function name.
	Name string
	// Arity is the function arity.
	Arity int
	// TypeSpec is the raw -spec string extracted from the <typespec> element.
	// Empty when no <typespec> is present.
	TypeSpec string
	// Doc is the plain-text documentation extracted from <p> children of
	// the <description> element.
	Doc string
}

// ModuleDoc is the result of parsing one EDoc XML file.
type ModuleDoc struct {
	// Name is the Erlang module name.
	Name string
	// Functions is the list of exported function documentation entries.
	Functions []FunctionDoc
}

// ParseXML parses an EDoc XML document from r and returns the extracted
// module documentation. Returns an error if the document is not valid EDoc
// XML; missing or empty fields are not errors.
func ParseXML(r io.Reader) (*ModuleDoc, error) {
	data, err := io.ReadAll(r)
	if err != nil {
		return nil, fmt.Errorf("edocingest: read: %w", err)
	}
	return ParseXMLBytes(data)
}

// ParseXMLBytes parses EDoc XML from raw bytes.
func ParseXMLBytes(data []byte) (*ModuleDoc, error) {
	dec := xml.NewDecoder(strings.NewReader(string(data)))
	var doc ModuleDoc
	var inFunctions bool
	var curFun *FunctionDoc
	var captureText string
	var captureTarget string // "typespec" | "doc"

	for {
		tok, err := dec.Token()
		if err == io.EOF {
			break
		}
		if err != nil {
			return nil, fmt.Errorf("edocingest: XML parse: %w", err)
		}
		switch t := tok.(type) {
		case xml.StartElement:
			switch t.Name.Local {
			case "module", "edoc":
				for _, a := range t.Attr {
					if a.Name.Local == "name" {
						doc.Name = a.Value
					}
				}
			case "functions":
				inFunctions = true
			case "function":
				if inFunctions {
					curFun = &FunctionDoc{}
					for _, a := range t.Attr {
						switch a.Name.Local {
						case "name":
							curFun.Name = a.Value
						case "arity":
							curFun.Arity = parseInt(a.Value)
						}
					}
				}
			case "typespec":
				if curFun != nil {
					captureTarget = "typespec"
					captureText = ""
				}
			case "p", "div":
				if curFun != nil && captureTarget == "" {
					captureTarget = "doc"
					captureText = ""
				}
			}
		case xml.EndElement:
			switch t.Name.Local {
			case "functions":
				inFunctions = false
			case "function":
				if curFun != nil {
					doc.Functions = append(doc.Functions, *curFun)
					curFun = nil
					captureTarget = ""
					captureText = ""
				}
			case "typespec":
				if captureTarget == "typespec" && curFun != nil {
					curFun.TypeSpec = strings.TrimSpace(captureText)
					captureTarget = ""
					captureText = ""
				}
			case "p", "div":
				if captureTarget == "doc" && curFun != nil {
					if curFun.Doc == "" {
						curFun.Doc = strings.TrimSpace(captureText)
					}
					captureTarget = ""
					captureText = ""
				}
			}
		case xml.CharData:
			if captureTarget != "" {
				captureText += string(t)
			}
		}
	}
	return &doc, nil
}

// FunctionsWithSpec returns only the FunctionDoc entries that have a non-empty
// TypeSpec. These are the only entries the typemap layer can attempt to translate.
func (m *ModuleDoc) FunctionsWithSpec() []FunctionDoc {
	var out []FunctionDoc
	for _, f := range m.Functions {
		if f.TypeSpec != "" {
			out = append(out, f)
		}
	}
	return out
}

// FunctionsWithoutSpec returns FunctionDoc entries that have no TypeSpec.
// These will produce SkipNoSpec skip reports in phase 4.
func (m *ModuleDoc) FunctionsWithoutSpec() []FunctionDoc {
	var out []FunctionDoc
	for _, f := range m.Functions {
		if f.TypeSpec == "" {
			out = append(out, f)
		}
	}
	return out
}

func parseInt(s string) int {
	n := 0
	for _, ch := range s {
		if ch < '0' || ch > '9' {
			break
		}
		n = n*10 + int(ch-'0')
	}
	return n
}
