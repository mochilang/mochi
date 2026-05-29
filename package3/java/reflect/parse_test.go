package reflect

import (
	"testing"
)

const fixtureSurfaceJSON = `{
  "schemaVersion": 1,
  "classes": [
    {
      "name": "com.google.common.base.Optional",
      "modifiers": ["public", "abstract"],
      "superclass": "java.lang.Object",
      "interfaces": [],
      "methods": [
        {
          "name": "of",
          "modifiers": ["public", "static"],
          "returnType": "com.google.common.base.Optional",
          "parameterTypes": ["java.lang.Object"],
          "parameterNames": ["reference"],
          "genericReturnType": "com.google.common.base.Optional<T>",
          "genericParameterTypes": ["T"],
          "exceptionTypes": [],
          "isVarargs": false,
          "isDeprecated": false
        },
        {
          "name": "isPresent",
          "modifiers": ["public", "abstract"],
          "returnType": "boolean",
          "parameterTypes": [],
          "exceptionTypes": [],
          "isVarargs": false,
          "isDeprecated": false
        }
      ],
      "fields": [
        {
          "name": "EMPTY",
          "modifiers": ["public", "static", "final"],
          "type": "com.google.common.base.Optional",
          "genericType": "com.google.common.base.Optional<?>"
        }
      ],
      "constructors": []
    }
  ]
}`

func TestParseSurface(t *testing.T) {
	s, err := ParseSurface([]byte(fixtureSurfaceJSON))
	if err != nil {
		t.Fatalf("ParseSurface: %v", err)
	}
	if len(s.Classes) != 1 {
		t.Errorf("len(Classes) = %d; want 1", len(s.Classes))
	}
	cls := s.Classes[0]
	if cls.Name != "com.google.common.base.Optional" {
		t.Errorf("Class.Name = %q; want com.google.common.base.Optional", cls.Name)
	}
	if len(cls.Methods) != 2 {
		t.Errorf("len(Methods) = %d; want 2", len(cls.Methods))
	}
	if cls.Methods[0].Name != "of" {
		t.Errorf("Methods[0].Name = %q; want of", cls.Methods[0].Name)
	}
	if len(cls.Fields) != 1 {
		t.Errorf("len(Fields) = %d; want 1", len(cls.Fields))
	}
}

func TestParseSurfaceWrongVersion(t *testing.T) {
	data := `{"schemaVersion": 2, "classes": []}`
	_, err := ParseSurface([]byte(data))
	if err == nil {
		t.Error("expected error for unsupported schema version")
	}
}

func TestParseSurfaceInvalidJSON(t *testing.T) {
	_, err := ParseSurface([]byte("not json"))
	if err == nil {
		t.Error("expected error for invalid JSON")
	}
}
