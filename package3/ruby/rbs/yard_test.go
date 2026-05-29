package rbs

import (
	"testing"
)

func TestParseYARDType(t *testing.T) {
	cases := []struct {
		name string
		in   string
		want TypeKind
		// for container types, validate elem/key/value kinds too
		elemKind  TypeKind
		keyKind   TypeKind
		valueKind TypeKind
	}{
		{"string", "String", TypeString, 0, 0, 0},
		{"integer", "Integer", TypeInteger, 0, 0, 0},
		{"float", "Float", TypeFloat, 0, 0, 0},
		{"symbol", "Symbol", TypeSymbol, 0, 0, 0},
		{"boolean_word", "Boolean", TypeBool, 0, 0, 0},
		{"true_false", "true, false", TypeBool, 0, 0, 0},
		{"false_true", "false, true", TypeBool, 0, 0, 0},
		{"nil", "nil", TypeNil, 0, 0, 0},
		{"void", "void", TypeVoid, 0, 0, 0},
		{"array_string", "Array<String>", TypeArray, TypeString, 0, 0},
		{"array_integer", "Array<Integer>", TypeArray, TypeInteger, 0, 0},
		{"array_float", "Array<Float>", TypeArray, TypeFloat, 0, 0},
		{"array_symbol", "Array<Symbol>", TypeArray, TypeSymbol, 0, 0},
		{"hash_brace", "Hash{Symbol => Integer}", TypeHash, 0, TypeSymbol, TypeInteger},
		{"hash_brace_str_str", "Hash{String => String}", TypeHash, 0, TypeString, TypeString},
		{"hash_angle", "Hash<String, Integer>", TypeHash, 0, TypeString, TypeInteger},
		{"optional_str_nil", "String, nil", TypeOptional, TypeString, 0, 0},
		{"optional_int_nil", "Integer, nil", TypeOptional, TypeInteger, 0, 0},
		{"optional_nil_str", "nil, String", TypeOptional, TypeString, 0, 0},
		{"optional_float_nil", "Float, nil", TypeOptional, TypeFloat, 0, 0},
		{"complex_union", "Integer, Float", TypeUnknown, 0, 0, 0},
		{"empty", "", TypeUnknown, 0, 0, 0},
		{"numeric_approx", "Numeric", TypeFloat, 0, 0, 0},
		{"unrecognised_class", "SomeClass", TypeUnknown, 0, 0, 0},
		{"unrecognised_generic", "SomeClass<String>", TypeUnknown, 0, 0, 0},
		{"whitespace_padding", "  String  ", TypeString, 0, 0, 0},
	}

	for _, tc := range cases {
		tc := tc
		t.Run(tc.name, func(t *testing.T) {
			got := ParseYARDType(tc.in)
			if got == nil {
				t.Fatalf("ParseYARDType(%q) = nil, want kind %v", tc.in, tc.want)
			}
			if got.Kind != tc.want {
				t.Errorf("ParseYARDType(%q).Kind = %v, want %v", tc.in, got.Kind, tc.want)
			}
			if tc.elemKind != 0 {
				if got.Elem == nil {
					t.Errorf("ParseYARDType(%q).Elem = nil, want kind %v", tc.in, tc.elemKind)
				} else if got.Elem.Kind != tc.elemKind {
					t.Errorf("ParseYARDType(%q).Elem.Kind = %v, want %v", tc.in, got.Elem.Kind, tc.elemKind)
				}
			}
			if tc.keyKind != 0 {
				if got.Key == nil {
					t.Errorf("ParseYARDType(%q).Key = nil, want kind %v", tc.in, tc.keyKind)
				} else if got.Key.Kind != tc.keyKind {
					t.Errorf("ParseYARDType(%q).Key.Kind = %v, want %v", tc.in, got.Key.Kind, tc.keyKind)
				}
			}
			if tc.valueKind != 0 {
				if got.Value == nil {
					t.Errorf("ParseYARDType(%q).Value = nil, want kind %v", tc.in, tc.valueKind)
				} else if got.Value.Kind != tc.valueKind {
					t.Errorf("ParseYARDType(%q).Value.Kind = %v, want %v", tc.in, got.Value.Kind, tc.valueKind)
				}
			}
		})
	}
}

func TestParseYARDDoc(t *testing.T) {
	fixture := []byte(`{
  "docobjects": [
    {
      "path": "Redis#get",
      "type": "method",
      "name": "get",
      "parameters": [["key", "String"]],
      "return_type": "String, nil",
      "namespace": {"path": "Redis", "type": "class"}
    },
    {
      "path": "Redis#set",
      "type": "method",
      "name": "set",
      "parameters": [["key", "String"], ["value", "String"]],
      "return_type": "String",
      "namespace": {"path": "Redis", "type": "class"}
    },
    {
      "path": "Redis#del",
      "type": "method",
      "name": "del",
      "parameters": [["key", "String"]],
      "return_type": "Integer",
      "namespace": {"path": "Redis", "type": "class"}
    },
    {
      "path": "Redis.new",
      "type": "method",
      "name": "new",
      "parameters": [["url", "String"]],
      "return_type": "Redis",
      "namespace": {"path": "Redis", "type": "class"}
    },
    {
      "path": "JSON.parse",
      "type": "method",
      "name": "parse",
      "parameters": [["source", "String"]],
      "return_type": "Hash{String => String}",
      "namespace": {"path": "JSON", "type": "module"}
    }
  ]
}`)

	surface, err := ParseYARDDoc("redis", "5.0.0", fixture)
	if err != nil {
		t.Fatalf("ParseYARDDoc returned error: %v", err)
	}

	if surface.Gem != "redis" {
		t.Errorf("Gem = %q, want %q", surface.Gem, "redis")
	}
	if surface.Version != "5.0.0" {
		t.Errorf("Version = %q, want %q", surface.Version, "5.0.0")
	}
	if surface.Source != SourceYARD {
		t.Errorf("Source = %v, want SourceYARD", surface.Source)
	}

	// Expect two classes: Redis and JSON.
	if len(surface.Classes) != 2 {
		t.Fatalf("len(Classes) = %d, want 2", len(surface.Classes))
	}

	redis := surface.Classes[0]
	if redis.Name != "Redis" {
		t.Errorf("Classes[0].Name = %q, want %q", redis.Name, "Redis")
	}
	if len(redis.Methods) != 4 {
		t.Fatalf("Redis methods = %d, want 4", len(redis.Methods))
	}

	// Validate Redis#get: returns optional string.
	get := redis.Methods[0]
	if get.Name != "get" {
		t.Errorf("method[0].Name = %q, want %q", get.Name, "get")
	}
	if len(get.Types) != 1 {
		t.Fatalf("get.Types len = %d, want 1", len(get.Types))
	}
	mt := get.Types[0]
	if mt.Return == nil || mt.Return.Kind != TypeOptional {
		t.Errorf("get return kind = %v, want TypeOptional", mt.Return)
	}
	if mt.Return.Elem == nil || mt.Return.Elem.Kind != TypeString {
		t.Errorf("get return elem kind = %v, want TypeString", mt.Return.Elem)
	}
	if len(mt.Params) != 1 || mt.Params[0].Name != "key" {
		t.Errorf("get params = %v, want [{key String}]", mt.Params)
	}

	// Validate Redis.new is singleton.
	newMethod := redis.Methods[3]
	if newMethod.Kind != MethodSingleton {
		t.Errorf("Redis.new kind = %v, want MethodSingleton", newMethod.Kind)
	}

	// Validate JSON class.
	jsonClass := surface.Classes[1]
	if jsonClass.Name != "JSON" {
		t.Errorf("Classes[1].Name = %q, want %q", jsonClass.Name, "JSON")
	}
	if len(jsonClass.Methods) != 1 {
		t.Fatalf("JSON methods = %d, want 1", len(jsonClass.Methods))
	}
	parseMethod := jsonClass.Methods[0]
	if parseMethod.Name != "parse" {
		t.Errorf("parse name = %q", parseMethod.Name)
	}
	retType := parseMethod.Types[0].Return
	if retType == nil || retType.Kind != TypeHash {
		t.Errorf("JSON.parse return kind = %v, want TypeHash", retType)
	}
}

func TestParseYARDDoc_InvalidJSON(t *testing.T) {
	_, err := ParseYARDDoc("mygem", "1.0.0", []byte(`not json`))
	if err == nil {
		t.Fatal("expected error for invalid JSON, got nil")
	}
}

func TestParseYARDDoc_Empty(t *testing.T) {
	surface, err := ParseYARDDoc("mygem", "1.0.0", []byte(`{"docobjects":[]}`))
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(surface.Classes) != 0 {
		t.Errorf("expected 0 classes, got %d", len(surface.Classes))
	}
	if surface.Source != SourceYARD {
		t.Errorf("Source = %v, want SourceYARD", surface.Source)
	}
}
