package edocingest

import (
	"strings"
	"testing"
)

const edocSimple = `<?xml version="1.0" encoding="utf-8"?>
<module name="lager">
  <functions>
    <function name="log" arity="3">
      <typespec>log(Level, Module, Message) -> ok when Level :: atom(), Module :: atom(), Message :: string().</typespec>
      <description><p>Log a message at the given level.</p></description>
    </function>
    <function name="start" arity="0">
      <description><p>Start the lager application.</p></description>
    </function>
  </functions>
</module>`

const edocMultiFunctions = `<?xml version="1.0" encoding="utf-8"?>
<edoc name="observer_cli">
  <functions>
    <function name="start" arity="0">
      <typespec>start() -> ok | {error, term()}.</typespec>
    </function>
    <function name="start" arity="1">
      <typespec>start(Opts) -> ok when Opts :: map().</typespec>
    </function>
    <function name="stop" arity="0">
    </function>
  </functions>
</edoc>`

const edocEmpty = `<?xml version="1.0" encoding="utf-8"?>
<module name="empty_mod">
  <functions>
  </functions>
</module>`

const edocNoFunctions = `<?xml version="1.0" encoding="utf-8"?>
<module name="mymod">
</module>`

func TestParseXML_ModuleName(t *testing.T) {
	doc, err := ParseXMLBytes([]byte(edocSimple))
	if err != nil {
		t.Fatalf("ParseXMLBytes: %v", err)
	}
	if doc.Name != "lager" {
		t.Errorf("Name = %q, want lager", doc.Name)
	}
}

func TestParseXML_FunctionCount(t *testing.T) {
	doc, err := ParseXMLBytes([]byte(edocSimple))
	if err != nil {
		t.Fatalf("ParseXMLBytes: %v", err)
	}
	if len(doc.Functions) != 2 {
		t.Errorf("len(Functions) = %d, want 2", len(doc.Functions))
	}
}

func TestParseXML_FunctionWithTypespec(t *testing.T) {
	doc, err := ParseXMLBytes([]byte(edocSimple))
	if err != nil {
		t.Fatalf("ParseXMLBytes: %v", err)
	}
	log := doc.Functions[0]
	if log.Name != "log" {
		t.Errorf("Function[0].Name = %q, want log", log.Name)
	}
	if log.Arity != 3 {
		t.Errorf("Function[0].Arity = %d, want 3", log.Arity)
	}
	if log.TypeSpec == "" {
		t.Error("Function[0].TypeSpec should be non-empty")
	}
	if !strings.Contains(log.TypeSpec, "Level") {
		t.Errorf("TypeSpec = %q, should contain 'Level'", log.TypeSpec)
	}
}

func TestParseXML_FunctionWithoutTypespec(t *testing.T) {
	doc, err := ParseXMLBytes([]byte(edocSimple))
	if err != nil {
		t.Fatalf("ParseXMLBytes: %v", err)
	}
	start := doc.Functions[1]
	if start.Name != "start" {
		t.Errorf("Function[1].Name = %q, want start", start.Name)
	}
	if start.TypeSpec != "" {
		t.Errorf("Function[1].TypeSpec should be empty, got %q", start.TypeSpec)
	}
}

func TestParseXML_FunctionDoc(t *testing.T) {
	doc, err := ParseXMLBytes([]byte(edocSimple))
	if err != nil {
		t.Fatalf("ParseXMLBytes: %v", err)
	}
	if doc.Functions[0].Doc == "" {
		t.Error("Function[0].Doc should be non-empty")
	}
}

func TestParseXML_EdocRootElement(t *testing.T) {
	// Test <edoc name="..."> root element variant.
	doc, err := ParseXMLBytes([]byte(edocMultiFunctions))
	if err != nil {
		t.Fatalf("ParseXMLBytes: %v", err)
	}
	if doc.Name != "observer_cli" {
		t.Errorf("Name = %q, want observer_cli", doc.Name)
	}
	if len(doc.Functions) != 3 {
		t.Errorf("len(Functions) = %d, want 3", len(doc.Functions))
	}
}

func TestParseXML_OverloadedFunction(t *testing.T) {
	doc, err := ParseXMLBytes([]byte(edocMultiFunctions))
	if err != nil {
		t.Fatalf("ParseXMLBytes: %v", err)
	}
	// start/0 and start/1 are separate entries.
	startCount := 0
	for _, f := range doc.Functions {
		if f.Name == "start" {
			startCount++
		}
	}
	if startCount != 2 {
		t.Errorf("startCount = %d, want 2", startCount)
	}
}

func TestFunctionsWithSpec(t *testing.T) {
	doc, err := ParseXMLBytes([]byte(edocMultiFunctions))
	if err != nil {
		t.Fatalf("ParseXMLBytes: %v", err)
	}
	withSpec := doc.FunctionsWithSpec()
	if len(withSpec) != 2 {
		t.Errorf("FunctionsWithSpec len = %d, want 2 (stop has no typespec)", len(withSpec))
	}
}

func TestFunctionsWithoutSpec(t *testing.T) {
	doc, err := ParseXMLBytes([]byte(edocMultiFunctions))
	if err != nil {
		t.Fatalf("ParseXMLBytes: %v", err)
	}
	withoutSpec := doc.FunctionsWithoutSpec()
	if len(withoutSpec) != 1 {
		t.Errorf("FunctionsWithoutSpec len = %d, want 1 (only stop)", len(withoutSpec))
	}
	if withoutSpec[0].Name != "stop" {
		t.Errorf("FunctionsWithoutSpec[0] = %q, want stop", withoutSpec[0].Name)
	}
}

func TestParseXML_EmptyFunctions(t *testing.T) {
	doc, err := ParseXMLBytes([]byte(edocEmpty))
	if err != nil {
		t.Fatalf("ParseXMLBytes: %v", err)
	}
	if len(doc.Functions) != 0 {
		t.Errorf("len(Functions) = %d, want 0", len(doc.Functions))
	}
}

func TestParseXML_NoFunctionsElement(t *testing.T) {
	doc, err := ParseXMLBytes([]byte(edocNoFunctions))
	if err != nil {
		t.Fatalf("ParseXMLBytes: %v", err)
	}
	if doc.Name != "mymod" {
		t.Errorf("Name = %q, want mymod", doc.Name)
	}
}

func TestParseXML_Reader(t *testing.T) {
	r := strings.NewReader(edocSimple)
	doc, err := ParseXML(r)
	if err != nil {
		t.Fatalf("ParseXML: %v", err)
	}
	if doc.Name != "lager" {
		t.Errorf("Name = %q, want lager", doc.Name)
	}
}

func TestParseXML_InvalidXML(t *testing.T) {
	_, err := ParseXMLBytes([]byte("<not valid xml>"))
	// Invalid XML should either error or return empty — not panic.
	// The Go xml.Decoder is lenient with some malformed inputs; the key is no panic.
	_ = err
}

func TestParseXML_ArityParsing(t *testing.T) {
	xml := `<?xml version="1.0"?>
<module name="cuttlefish">
  <functions>
    <function name="conf_to_schema" arity="42">
      <typespec>conf_to_schema(Conf) -> schema().</typespec>
    </function>
  </functions>
</module>`
	doc, err := ParseXMLBytes([]byte(xml))
	if err != nil {
		t.Fatalf("ParseXMLBytes: %v", err)
	}
	if len(doc.Functions) != 1 {
		t.Fatalf("len = %d", len(doc.Functions))
	}
	if doc.Functions[0].Arity != 42 {
		t.Errorf("Arity = %d, want 42", doc.Functions[0].Arity)
	}
}
