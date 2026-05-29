package beamingest

import (
	"encoding/binary"
	"testing"

	"mochi/package3/erlang/etf"
)

// buildBeam constructs a minimal valid BEAM file from a map of chunk ID ->
// raw chunk data (not ETF-encoded; callers pass ready-to-use bytes).
func buildBeam(chunks map[string][]byte) []byte {
	// Build the IFF chunk sequence.
	var body []byte
	for id, data := range chunks {
		if len(id) != 4 {
			panic("chunk id must be 4 bytes: " + id)
		}
		chunk := make([]byte, 8+len(data))
		copy(chunk[0:4], id)
		binary.BigEndian.PutUint32(chunk[4:8], uint32(len(data)))
		copy(chunk[8:], data)
		// Pad to 4-byte boundary.
		if pad := len(data) % 4; pad != 0 {
			chunk = append(chunk, make([]byte, 4-pad)...)
		}
		body = append(body, chunk...)
	}

	// FOR1 header: "FOR1" + uint32(len(body)+4) + "BEAM"
	header := make([]byte, 12)
	copy(header[0:4], "FOR1")
	binary.BigEndian.PutUint32(header[4:8], uint32(len(body)+4))
	copy(header[8:12], "BEAM")
	return append(header, body...)
}

// encodeDbgi encodes a Dbgi chunk value: {debug_info_v1, erl_abstract_code, {Forms, []}}
func encodeDbgi(forms etf.List) []byte {
	inner := etf.Tuple{forms, etf.List{}}
	term := etf.Tuple{etf.Atom("debug_info_v1"), etf.Atom("erl_abstract_code"), inner}
	b, err := etf.Encode(term)
	if err != nil {
		panic(err)
	}
	return b
}

// encodeAbst encodes a legacy Abst chunk: plain ETF list of forms.
func encodeAbst(forms etf.List) []byte {
	b, err := etf.Encode(forms)
	if err != nil {
		panic(err)
	}
	return b
}

// moduleAttr returns a {attribute, 1, module, Name} AST form.
func moduleAttr(name string) etf.Tuple {
	return etf.Tuple{etf.Atom("attribute"), 1, etf.Atom("module"), etf.Atom(name)}
}

// specAttr returns a {attribute, 1, spec, {{Fun,Arity},[Clause]}} form.
func specAttr(fun string, arity int, clause interface{}) etf.Tuple {
	sig := etf.Tuple{etf.Atom(fun), arity}
	val := etf.Tuple{sig, etf.List{clause}}
	return etf.Tuple{etf.Atom("attribute"), 1, etf.Atom("spec"), val}
}

// typeAttr returns a {attribute, 1, type, {Name, Body, []}} form.
func typeAttr(name string, body interface{}) etf.Tuple {
	val := etf.Tuple{etf.Atom(name), body, etf.List{}}
	return etf.Tuple{etf.Atom("attribute"), 1, etf.Atom("type"), val}
}

// opaqueAttr returns a {attribute, 1, opaque, {Name, Body, []}} form.
func opaqueAttr(name string, body interface{}) etf.Tuple {
	val := etf.Tuple{etf.Atom(name), body, etf.List{}}
	return etf.Tuple{etf.Atom("attribute"), 1, etf.Atom("opaque"), val}
}

// A simple clause placeholder representing (integer()) -> ok.
var simpleClause = etf.Tuple{
	etf.Atom("type"), 1, etf.Atom("fun"),
	etf.List{
		etf.Tuple{etf.Atom("type"), 1, etf.Atom("product"),
			etf.List{etf.Tuple{etf.Atom("type"), 1, etf.Atom("integer"), etf.List{}}}},
		etf.Tuple{etf.Atom("atom"), 1, etf.Atom("ok")},
	},
}

func TestParseBeam_InvalidMagic(t *testing.T) {
	_, err := ParseBeam([]byte("NOTABEAMFILE"))
	if err == nil {
		t.Error("ParseBeam should fail for non-BEAM data")
	}
}

func TestParseBeam_TooShort(t *testing.T) {
	_, err := ParseBeam([]byte("FOR1"))
	if err == nil {
		t.Error("ParseBeam should fail for truncated file")
	}
}

func TestParseBeam_NoAbstractCode(t *testing.T) {
	beam := buildBeam(map[string][]byte{
		// A real beam file always has an Atom chunk; we don't parse it but
		// this exercises the "no Dbgi/Abst" path.
		"Atom": {0x00, 0x00, 0x00, 0x01, 0x05, 'h', 'e', 'l', 'l', 'o'},
	})
	mod, err := ParseBeam(beam)
	if err != nil {
		t.Fatalf("ParseBeam: %v", err)
	}
	if mod.HasAbstractCode {
		t.Error("HasAbstractCode should be false when neither Dbgi nor Abst is present")
	}
	if len(mod.Specs) != 0 || len(mod.Types) != 0 {
		t.Error("no specs or types expected without abstract code")
	}
}

func TestParseBeam_Dbgi_ModuleName(t *testing.T) {
	forms := etf.List{moduleAttr("cowboy")}
	beam := buildBeam(map[string][]byte{"Dbgi": encodeDbgi(forms)})
	mod, err := ParseBeam(beam)
	if err != nil {
		t.Fatalf("ParseBeam: %v", err)
	}
	if mod.Name != "cowboy" {
		t.Errorf("Name = %q, want cowboy", mod.Name)
	}
	if !mod.HasAbstractCode {
		t.Error("HasAbstractCode should be true")
	}
}

func TestParseBeam_Dbgi_Spec(t *testing.T) {
	forms := etf.List{
		moduleAttr("ranch"),
		specAttr("start_listener", 4, simpleClause),
	}
	beam := buildBeam(map[string][]byte{"Dbgi": encodeDbgi(forms)})
	mod, err := ParseBeam(beam)
	if err != nil {
		t.Fatalf("ParseBeam: %v", err)
	}
	if mod.Name != "ranch" {
		t.Errorf("Name = %q, want ranch", mod.Name)
	}
	if len(mod.Specs) != 1 {
		t.Fatalf("len(Specs) = %d, want 1", len(mod.Specs))
	}
	s := mod.Specs[0]
	if s.Function != "start_listener" {
		t.Errorf("Function = %q, want start_listener", s.Function)
	}
	if s.Arity != 4 {
		t.Errorf("Arity = %d, want 4", s.Arity)
	}
	if s.Module != "ranch" {
		t.Errorf("Module = %q, want ranch", s.Module)
	}
	if len(s.Clauses) != 1 {
		t.Errorf("Clauses len = %d, want 1", len(s.Clauses))
	}
}

func TestParseBeam_Dbgi_MultipleSpecs(t *testing.T) {
	forms := etf.List{
		moduleAttr("hackney"),
		specAttr("get", 2, simpleClause),
		specAttr("post", 3, simpleClause),
		specAttr("put", 3, simpleClause),
	}
	beam := buildBeam(map[string][]byte{"Dbgi": encodeDbgi(forms)})
	mod, err := ParseBeam(beam)
	if err != nil {
		t.Fatalf("ParseBeam: %v", err)
	}
	if len(mod.Specs) != 3 {
		t.Errorf("len(Specs) = %d, want 3", len(mod.Specs))
	}
}

func TestParseBeam_Dbgi_Type(t *testing.T) {
	body := etf.Tuple{etf.Atom("type"), 1, etf.Atom("integer"), etf.List{}}
	forms := etf.List{
		moduleAttr("jsx"),
		typeAttr("json_term", body),
	}
	beam := buildBeam(map[string][]byte{"Dbgi": encodeDbgi(forms)})
	mod, err := ParseBeam(beam)
	if err != nil {
		t.Fatalf("ParseBeam: %v", err)
	}
	if len(mod.Types) != 1 {
		t.Fatalf("len(Types) = %d, want 1", len(mod.Types))
	}
	ty := mod.Types[0]
	if ty.Name != "json_term" {
		t.Errorf("Type.Name = %q, want json_term", ty.Name)
	}
	if ty.Opaque {
		t.Error("should not be opaque")
	}
}

func TestParseBeam_Dbgi_OpaqueType(t *testing.T) {
	body := etf.Tuple{etf.Atom("type"), 1, etf.Atom("any"), etf.List{}}
	forms := etf.List{
		moduleAttr("jose_jws"),
		opaqueAttr("t", body),
	}
	beam := buildBeam(map[string][]byte{"Dbgi": encodeDbgi(forms)})
	mod, err := ParseBeam(beam)
	if err != nil {
		t.Fatalf("ParseBeam: %v", err)
	}
	if len(mod.Types) != 1 {
		t.Fatalf("len(Types) = %d, want 1", len(mod.Types))
	}
	if !mod.Types[0].Opaque {
		t.Error("Opaque should be true")
	}
}

func TestParseBeam_Abst_Legacy(t *testing.T) {
	forms := etf.List{
		moduleAttr("poolboy"),
		specAttr("checkout", 1, simpleClause),
	}
	beam := buildBeam(map[string][]byte{"Abst": encodeAbst(forms)})
	mod, err := ParseBeam(beam)
	if err != nil {
		t.Fatalf("ParseBeam: %v", err)
	}
	if !mod.HasAbstractCode {
		t.Error("HasAbstractCode should be true for Abst chunk")
	}
	if mod.Name != "poolboy" {
		t.Errorf("Name = %q, want poolboy", mod.Name)
	}
	if len(mod.Specs) != 1 {
		t.Errorf("len(Specs) = %d, want 1", len(mod.Specs))
	}
}

func TestParseBeam_Dbgi_PrefersOverAbst(t *testing.T) {
	// Both chunks present: Dbgi should win.
	dbgiForms := etf.List{moduleAttr("cowlib")}
	abstForms := etf.List{moduleAttr("WRONG_abst")}
	beam := buildBeam(map[string][]byte{
		"Dbgi": encodeDbgi(dbgiForms),
		"Abst": encodeAbst(abstForms),
	})
	mod, err := ParseBeam(beam)
	if err != nil {
		t.Fatalf("ParseBeam: %v", err)
	}
	if mod.Name != "cowlib" {
		t.Errorf("Dbgi should win; Name = %q, want cowlib", mod.Name)
	}
}

func TestParseBeam_Dbgi_NoneNoCode(t *testing.T) {
	// {debug_info_v1, erl_abstract_code, none} — no_debug_info compile option.
	term := etf.Tuple{etf.Atom("debug_info_v1"), etf.Atom("erl_abstract_code"), etf.Atom("none")}
	b, _ := etf.Encode(term)
	beam := buildBeam(map[string][]byte{"Dbgi": b})
	mod, err := ParseBeam(beam)
	if err != nil {
		t.Fatalf("ParseBeam: %v", err)
	}
	if !mod.HasAbstractCode {
		// HasAbstractCode is true (chunk exists) even if content is 'none'.
		t.Error("HasAbstractCode should be true when Dbgi chunk exists")
	}
	if len(mod.Specs) != 0 {
		t.Error("no specs expected when Dbgi is 'none'")
	}
}

func TestParseBeam_TypeWithParams(t *testing.T) {
	// -type dict(K, V) :: ...
	params := etf.List{
		etf.Tuple{etf.Atom("var"), 1, etf.Atom("K")},
		etf.Tuple{etf.Atom("var"), 1, etf.Atom("V")},
	}
	body := etf.Tuple{etf.Atom("type"), 1, etf.Atom("any"), etf.List{}}
	val := etf.Tuple{etf.Atom("dict"), body, params}
	form := etf.Tuple{etf.Atom("attribute"), 1, etf.Atom("type"), val}
	forms := etf.List{moduleAttr("dict"), form}
	beam := buildBeam(map[string][]byte{"Dbgi": encodeDbgi(forms)})
	mod, err := ParseBeam(beam)
	if err != nil {
		t.Fatalf("ParseBeam: %v", err)
	}
	if len(mod.Types) != 1 {
		t.Fatalf("len(Types) = %d, want 1", len(mod.Types))
	}
	if mod.Types[0].Arity != 2 {
		t.Errorf("Type arity = %d, want 2", mod.Types[0].Arity)
	}
}

func TestParseBeam_RemoteSpec(t *testing.T) {
	// -spec Module:Fun/Arity (callback spec in behaviour).
	sig := etf.Tuple{etf.Atom("cowboy_handler"), etf.Atom("init"), 2}
	val := etf.Tuple{sig, etf.List{simpleClause}}
	form := etf.Tuple{etf.Atom("attribute"), 1, etf.Atom("spec"), val}
	forms := etf.List{moduleAttr("cowboy_handler"), form}
	beam := buildBeam(map[string][]byte{"Dbgi": encodeDbgi(forms)})
	mod, err := ParseBeam(beam)
	if err != nil {
		t.Fatalf("ParseBeam: %v", err)
	}
	if len(mod.Specs) != 1 {
		t.Fatalf("len(Specs) = %d, want 1", len(mod.Specs))
	}
	s := mod.Specs[0]
	if s.Module != "cowboy_handler" {
		t.Errorf("remote spec module = %q, want cowboy_handler", s.Module)
	}
	if s.Function != "init" {
		t.Errorf("remote spec fun = %q, want init", s.Function)
	}
	if s.Arity != 2 {
		t.Errorf("remote spec arity = %d, want 2", s.Arity)
	}
}
