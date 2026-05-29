package reflect

import (
	"testing"
)

func TestSurfaceTypes(t *testing.T) {
	s := &Surface{
		SchemaVersion: 1,
		Classes: []ClassInfo{
			{
				Name:      "com.example.Foo",
				Modifiers: []string{"public"},
				Methods: []MethodInfo{
					{
						Name:           "bar",
						Modifiers:      []string{"public"},
						ReturnType:     "java.lang.String",
						ParameterTypes: []string{},
						ExceptionTypes: []string{},
					},
				},
				Fields: []FieldInfo{
					{Name: "BAZ", Modifiers: []string{"public", "static", "final"}, Type: "int"},
				},
				Constructors: []ConstructorInfo{},
			},
		},
	}
	if len(s.Classes) != 1 {
		t.Errorf("len(Classes) = %d; want 1", len(s.Classes))
	}
	if len(s.Classes[0].Methods) != 1 {
		t.Errorf("len(Methods) = %d; want 1", len(s.Classes[0].Methods))
	}
	if len(s.Classes[0].Fields) != 1 {
		t.Errorf("len(Fields) = %d; want 1", len(s.Classes[0].Fields))
	}
}
