package metacli_test

import (
	"strings"
	"testing"

	"mochi/package3/dotnet/metacli"
)

const fixtureJSON = `{
  "assembly": "Newtonsoft.Json",
  "version": "13.0.1",
  "target_framework": "net6.0",
  "types": [
    {
      "namespace": "Newtonsoft.Json",
      "name": "JsonConvert",
      "kind": "class",
      "is_static": true,
      "methods": [
        {
          "name": "SerializeObject",
          "is_static": true,
          "return_type": {"full_name": "System.String", "kind": "primitive"},
          "parameters": [
            {"name": "value", "type": {"full_name": "System.Object", "kind": "class"}}
          ],
          "xml_doc": "Serializes the specified object to a JSON string."
        }
      ]
    },
    {
      "namespace": "Newtonsoft.Json",
      "name": "JsonSerializer",
      "kind": "class",
      "properties": [
        {
          "name": "NullValueHandling",
          "type": {"full_name": "Newtonsoft.Json.NullValueHandling", "kind": "enum"},
          "has_getter": true,
          "has_setter": true
        }
      ]
    },
    {
      "namespace": "Newtonsoft.Json",
      "name": "NullValueHandling",
      "kind": "enum",
      "enum_values": [
        {"name": "Include", "value": 0},
        {"name": "Ignore", "value": 1}
      ]
    },
    {
      "namespace": "Newtonsoft.Json.Linq",
      "name": "JToken",
      "kind": "class",
      "is_abstract": true
    },
    {
      "namespace": "Newtonsoft.Json",
      "name": "IJsonLineInfo",
      "kind": "interface"
    }
  ]
}`

func TestParse_topLevel(t *testing.T) {
	meta, err := metacli.Parse(strings.NewReader(fixtureJSON))
	if err != nil {
		t.Fatalf("Parse error: %v", err)
	}
	if meta.Assembly != "Newtonsoft.Json" {
		t.Errorf("Assembly = %q; want %q", meta.Assembly, "Newtonsoft.Json")
	}
	if meta.Version != "13.0.1" {
		t.Errorf("Version = %q; want %q", meta.Version, "13.0.1")
	}
	if meta.TargetFramework != "net6.0" {
		t.Errorf("TargetFramework = %q; want %q", meta.TargetFramework, "net6.0")
	}
}

func TestParse_typeCount(t *testing.T) {
	meta, _ := metacli.Parse(strings.NewReader(fixtureJSON))
	if len(meta.Types) != 5 {
		t.Errorf("got %d types; want 5", len(meta.Types))
	}
}

func TestParse_classType(t *testing.T) {
	meta, _ := metacli.Parse(strings.NewReader(fixtureJSON))
	td := meta.Types[0]
	if td.Name != "JsonConvert" {
		t.Errorf("Name = %q; want JsonConvert", td.Name)
	}
	if td.Kind != metacli.KindClass {
		t.Errorf("Kind = %q; want class", td.Kind)
	}
	if !td.IsStatic {
		t.Error("JsonConvert should be static")
	}
}

func TestParse_method(t *testing.T) {
	meta, _ := metacli.Parse(strings.NewReader(fixtureJSON))
	td := meta.Types[0]
	if len(td.Methods) != 1 {
		t.Fatalf("got %d methods; want 1", len(td.Methods))
	}
	m := td.Methods[0]
	if m.Name != "SerializeObject" {
		t.Errorf("Method name = %q; want SerializeObject", m.Name)
	}
	if m.ReturnType.FullName != "System.String" {
		t.Errorf("ReturnType.FullName = %q; want System.String", m.ReturnType.FullName)
	}
	if len(m.Parameters) != 1 {
		t.Fatalf("got %d params; want 1", len(m.Parameters))
	}
}

func TestParse_property(t *testing.T) {
	meta, _ := metacli.Parse(strings.NewReader(fixtureJSON))
	td := meta.Types[1]
	if len(td.Properties) != 1 {
		t.Fatalf("got %d properties; want 1", len(td.Properties))
	}
	p := td.Properties[0]
	if p.Name != "NullValueHandling" {
		t.Errorf("Prop name = %q; want NullValueHandling", p.Name)
	}
}

func TestParse_enum(t *testing.T) {
	meta, _ := metacli.Parse(strings.NewReader(fixtureJSON))
	td := meta.Types[2]
	if td.Kind != metacli.KindEnum {
		t.Errorf("Kind = %q; want enum", td.Kind)
	}
	if len(td.EnumValues) != 2 {
		t.Fatalf("got %d enum values; want 2", len(td.EnumValues))
	}
	if td.EnumValues[0].Name != "Include" {
		t.Errorf("EnumValue[0].Name = %q; want Include", td.EnumValues[0].Name)
	}
	if td.EnumValues[1].Value != 1 {
		t.Errorf("EnumValue[1].Value = %d; want 1", td.EnumValues[1].Value)
	}
}

func TestParse_abstractClass(t *testing.T) {
	meta, _ := metacli.Parse(strings.NewReader(fixtureJSON))
	td := meta.Types[3]
	if !td.IsAbstract {
		t.Error("JToken should be abstract")
	}
}

func TestParse_interface(t *testing.T) {
	meta, _ := metacli.Parse(strings.NewReader(fixtureJSON))
	td := meta.Types[4]
	if td.Kind != metacli.KindInterface {
		t.Errorf("Kind = %q; want interface", td.Kind)
	}
}

func TestParseBytes(t *testing.T) {
	meta, err := metacli.ParseBytes([]byte(fixtureJSON))
	if err != nil {
		t.Fatalf("ParseBytes error: %v", err)
	}
	if meta.Assembly != "Newtonsoft.Json" {
		t.Errorf("Assembly = %q", meta.Assembly)
	}
}

func TestParse_emptyJSON(t *testing.T) {
	_, err := metacli.Parse(strings.NewReader(""))
	if err == nil {
		t.Error("empty JSON should return error")
	}
}

func TestParse_invalidJSON(t *testing.T) {
	_, err := metacli.Parse(strings.NewReader("{not json}"))
	if err == nil {
		t.Error("invalid JSON should return error")
	}
}

func TestParse_methodXmlDoc(t *testing.T) {
	meta, _ := metacli.Parse(strings.NewReader(fixtureJSON))
	m := meta.Types[0].Methods[0]
	if m.XmlDoc == "" {
		t.Error("method XmlDoc should be non-empty")
	}
}

func TestParse_paramType(t *testing.T) {
	meta, _ := metacli.Parse(strings.NewReader(fixtureJSON))
	p := meta.Types[0].Methods[0].Parameters[0]
	if p.Type.Kind != metacli.TypeRefClass {
		t.Errorf("param type kind = %q; want class", p.Type.Kind)
	}
}

func TestParse_genericTypeFixture(t *testing.T) {
	const genericJSON = `{
  "assembly": "System.Collections",
  "version": "6.0.0",
  "target_framework": "net6.0",
  "types": [
    {
      "namespace": "System.Collections.Generic",
      "name": "List` + "`" + `1",
      "kind": "class",
      "is_generic": true,
      "type_params": ["T"],
      "methods": [
        {
          "name": "Add",
          "return_type": {"full_name": "System.Void", "kind": "void"},
          "parameters": [
            {"name": "item", "type": {"full_name": "T", "kind": "generic_param"}}
          ]
        }
      ]
    }
  ]
}`
	meta, err := metacli.Parse(strings.NewReader(genericJSON))
	if err != nil {
		t.Fatalf("Parse generic fixture: %v", err)
	}
	td := meta.Types[0]
	if !td.IsGeneric {
		t.Error("List`1 should be generic")
	}
	if len(td.TypeParams) != 1 || td.TypeParams[0] != "T" {
		t.Errorf("TypeParams = %v; want [T]", td.TypeParams)
	}
}
