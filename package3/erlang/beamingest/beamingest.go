// Package beamingest reads Erlang .beam files and extracts the type
// information the MEP-66 bridge needs to generate Mochi extern bindings.
//
// BEAM file format:
//
//	FOR1 header (4 bytes "FOR1" + big-endian uint32 of remaining length + "BEAM")
//	Followed by zero or more IFF chunks:
//	    chunk-id   [4 bytes ASCII]
//	    chunk-len  [uint32 big-endian]
//	    chunk-data [chunk-len bytes, padded to 4-byte boundary]
//
// Relevant chunks:
//
//   - "Dbgi" (OTP 20+): debug_info. Contains ETF-encoded abstract code that
//     includes all -spec and -type directives. Format:
//     {debug_info_v1, erl_abstract_code, {Forms, CompileOpts}} where Forms
//     is a list of Erlang AST forms (see erl_syntax / erl_parse).
//
//   - "Abst" (OTP 17-19, legacy): raw ETF of the same AST. Deprecated
//     but still emitted by some older packages.
//
//   - "Atom" / "AtU8": atom table (used for fallback when Dbgi is absent).
//
// Spec extraction:
// A SpecForm is yielded for each -spec attribute in the AST. Each SpecForm
// records the module, function name, arity, and the raw Erlang type terms
// for each clause (normally one clause, occasionally multiple).
//
// Type extraction:
// A TypeForm is yielded for each -type or -opaque attribute.
package beamingest

import (
	"encoding/binary"
	"fmt"
	"io"

	"mochi/package3/erlang/etf"
)

// SpecForm is a parsed -spec attribute extracted from BEAM abstract code.
type SpecForm struct {
	// Module is the module that owns the spec (from the -module attribute).
	Module string
	// Function is the exported function name.
	Function string
	// Arity is the function arity.
	Arity int
	// Clauses holds the raw ETF for each type clause.
	// Most specs have exactly one clause; overloaded specs have multiple.
	Clauses []interface{}
}

// TypeForm is a parsed -type or -opaque attribute.
type TypeForm struct {
	// Module is the module that owns the type.
	Module string
	// Name is the type name.
	Name string
	// Arity is the number of type parameters.
	Arity int
	// Opaque is true for -opaque declarations.
	Opaque bool
	// Body is the raw ETF of the type body.
	Body interface{}
}

// Module holds all the information extracted from a single .beam file.
type Module struct {
	// Name is the OTP module name (from the -module attribute).
	Name string
	// Specs is the list of -spec declarations found in the file.
	Specs []SpecForm
	// Types is the list of -type / -opaque declarations.
	Types []TypeForm
	// HasAbstractCode is true when the file contained a Dbgi or Abst chunk.
	HasAbstractCode bool
}

// ReadBeam parses r as a BEAM file and extracts module name, specs, and types.
// r must be positioned at the start of the file.
func ReadBeam(r io.Reader) (*Module, error) {
	data, err := io.ReadAll(r)
	if err != nil {
		return nil, fmt.Errorf("beamingest: read: %w", err)
	}
	return ParseBeam(data)
}

// ParseBeam parses a BEAM file from raw bytes.
func ParseBeam(data []byte) (*Module, error) {
	if len(data) < 12 {
		return nil, fmt.Errorf("beamingest: file too short (%d bytes)", len(data))
	}
	if string(data[0:4]) != "FOR1" {
		return nil, fmt.Errorf("beamingest: not a BEAM file (missing FOR1 magic)")
	}
	// Bytes 4-7: big-endian uint32 of remaining content length (after the header).
	// Bytes 8-11: "BEAM"
	if string(data[8:12]) != "BEAM" {
		return nil, fmt.Errorf("beamingest: not a BEAM file (missing BEAM marker)")
	}

	chunks, err := parseChunks(data[12:])
	if err != nil {
		return nil, fmt.Errorf("beamingest: parse chunks: %w", err)
	}

	mod := &Module{}

	// Prefer Dbgi over Abst.
	if dbgi, ok := chunks["Dbgi"]; ok {
		mod.HasAbstractCode = true
		if err := extractDbgi(dbgi, mod); err != nil {
			return nil, fmt.Errorf("beamingest: extract Dbgi: %w", err)
		}
	} else if abst, ok := chunks["Abst"]; ok {
		mod.HasAbstractCode = true
		if err := extractAbst(abst, mod); err != nil {
			return nil, fmt.Errorf("beamingest: extract Abst: %w", err)
		}
	}

	return mod, nil
}

// parseChunks reads the IFF chunk sequence from the BEAM body (after FOR1/BEAM header).
func parseChunks(body []byte) (map[string][]byte, error) {
	chunks := make(map[string][]byte)
	pos := 0
	for pos+8 <= len(body) {
		id := string(body[pos : pos+4])
		size := int(binary.BigEndian.Uint32(body[pos+4 : pos+8]))
		pos += 8
		if pos+size > len(body) {
			return nil, fmt.Errorf("beamingest: chunk %q claims %d bytes but only %d remain", id, size, len(body)-pos)
		}
		chunks[id] = body[pos : pos+size]
		// Advance past data, aligned to 4-byte boundary.
		aligned := (size + 3) &^ 3
		pos += aligned
	}
	return chunks, nil
}

// extractDbgi decodes the "Dbgi" chunk and populates mod.
//
// The Dbgi chunk contains a single ETF term:
//
//	{debug_info_v1, erl_abstract_code, {Forms, _CompileOpts}}
//
// where Forms is a list of Erlang AST form tuples.
func extractDbgi(data []byte, mod *Module) error {
	term, err := etf.Decode(data)
	if err != nil {
		return fmt.Errorf("ETF decode: %w", err)
	}
	outer, ok := term.(etf.Tuple)
	if !ok || len(outer) < 3 {
		return fmt.Errorf("unexpected Dbgi shape: %T", term)
	}
	// outer[0] == debug_info_v1
	// outer[1] == erl_abstract_code
	// outer[2] == {Forms, CompileOpts} or the atom 'none'
	inner := outer[2]
	if atom, isAtom := inner.(etf.Atom); isAtom && atom == "none" {
		// No abstract code (compiled with no_debug_info).
		return nil
	}
	innerTuple, ok := inner.(etf.Tuple)
	if !ok || len(innerTuple) < 1 {
		return fmt.Errorf("unexpected Dbgi inner shape: %T", inner)
	}
	forms, ok := innerTuple[0].(etf.List)
	if !ok {
		return fmt.Errorf("Dbgi forms is not a list: %T", innerTuple[0])
	}
	return walkForms(forms, mod)
}

// extractAbst decodes the legacy "Abst" chunk. The chunk contains a
// raw ETF list of AST forms (no wrapping tuple).
func extractAbst(data []byte, mod *Module) error {
	term, err := etf.Decode(data)
	if err != nil {
		return fmt.Errorf("ETF decode: %w", err)
	}
	forms, ok := term.(etf.List)
	if !ok {
		return fmt.Errorf("Abst is not a list: %T", term)
	}
	return walkForms(forms, mod)
}

// walkForms iterates over the list of AST form tuples and populates mod
// with the module name, specs, and types.
//
// Relevant form shapes:
//
//	{attribute, Line, module, ModuleName}
//	{attribute, Line, spec, {{Fun, Arity}, [Clause...]}}
//	{attribute, Line, type, {TypeName, TypeBody, TypeParams}}
//	{attribute, Line, opaque, {TypeName, TypeBody, TypeParams}}
func walkForms(forms etf.List, mod *Module) error {
	for _, raw := range forms {
		form, ok := raw.(etf.Tuple)
		if !ok || len(form) < 3 {
			continue
		}
		kind, ok := form[0].(etf.Atom)
		if !ok || kind != "attribute" {
			continue
		}
		attr, ok := form[2].(etf.Atom)
		if !ok {
			continue
		}
		switch attr {
		case "module":
			if len(form) >= 4 {
				mod.Name = atomOrString(form[3])
			}
		case "spec":
			if len(form) >= 4 {
				if spec, err := parseSpec(form[3], mod.Name); err == nil {
					mod.Specs = append(mod.Specs, spec)
				}
			}
		case "type":
			if len(form) >= 4 {
				if typ, err := parseType(form[3], mod.Name, false); err == nil {
					mod.Types = append(mod.Types, typ)
				}
			}
		case "opaque":
			if len(form) >= 4 {
				if typ, err := parseType(form[3], mod.Name, true); err == nil {
					mod.Types = append(mod.Types, typ)
				}
			}
		}
	}
	return nil
}

// parseSpec parses a -spec attribute value.
//
// The value is either:
//
//	{{Fun, Arity}, [Clause...]}          — local spec
//	{{Module, Fun, Arity}, [Clause...]}  — remote spec (callback in behaviour)
func parseSpec(val interface{}, moduleName string) (SpecForm, error) {
	outer, ok := val.(etf.Tuple)
	if !ok || len(outer) != 2 {
		return SpecForm{}, fmt.Errorf("spec val not 2-tuple: %T", val)
	}
	sig, ok := outer[0].(etf.Tuple)
	if !ok {
		return SpecForm{}, fmt.Errorf("spec sig not tuple: %T", outer[0])
	}
	clauses, ok := outer[1].(etf.List)
	if !ok {
		return SpecForm{}, fmt.Errorf("spec clauses not list: %T", outer[1])
	}
	var mod, fun string
	var arity int
	switch len(sig) {
	case 2:
		fun = atomOrString(sig[0])
		arity = toInt(sig[1])
		mod = moduleName
	case 3:
		mod = atomOrString(sig[0])
		fun = atomOrString(sig[1])
		arity = toInt(sig[2])
	default:
		return SpecForm{}, fmt.Errorf("spec sig has %d elements", len(sig))
	}
	sf := SpecForm{Module: mod, Function: fun, Arity: arity}
	for _, c := range clauses {
		sf.Clauses = append(sf.Clauses, c)
	}
	return sf, nil
}

// parseType parses a -type or -opaque attribute value.
//
// The value is: {TypeName, TypeBody, [TypeParam...]}
func parseType(val interface{}, moduleName string, opaque bool) (TypeForm, error) {
	t, ok := val.(etf.Tuple)
	if !ok || len(t) < 3 {
		return TypeForm{}, fmt.Errorf("type val not 3-tuple: %T len=%d", val, lenTuple(val))
	}
	name := atomOrString(t[0])
	body := t[1]
	params, _ := t[2].(etf.List)
	return TypeForm{
		Module: moduleName,
		Name:   name,
		Arity:  len(params),
		Opaque: opaque,
		Body:   body,
	}, nil
}

func atomOrString(v interface{}) string {
	switch t := v.(type) {
	case etf.Atom:
		return string(t)
	case string:
		return t
	case []byte:
		return string(t)
	}
	return fmt.Sprintf("%v", v)
}

func toInt(v interface{}) int {
	switch t := v.(type) {
	case int:
		return t
	case int64:
		return int(t)
	case uint:
		return int(t)
	}
	return 0
}

func lenTuple(v interface{}) int {
	if t, ok := v.(etf.Tuple); ok {
		return len(t)
	}
	return 0
}
