package wrapper

import (
	"testing"

	javareflect "mochi/package3/java/reflect"
	"mochi/package3/java/typemap"
)

func fixtureClassInfo() javareflect.ClassInfo {
	return javareflect.ClassInfo{
		Name:      "com.example.Greeter",
		Modifiers: []string{"public"},
		Methods: []javareflect.MethodInfo{
			{
				Name:           "greet",
				Modifiers:      []string{"public"},
				ReturnType:     "java.lang.String",
				ParameterTypes: []string{"java.lang.String"},
				ParameterNames: []string{"name"},
				ExceptionTypes: []string{},
			},
			{
				Name:           "count",
				Modifiers:      []string{"public", "static"},
				ReturnType:     "int",
				ParameterTypes: []string{},
				ExceptionTypes: []string{},
			},
			{
				Name:           "variadics",
				Modifiers:      []string{"public"},
				ReturnType:     "void",
				ParameterTypes: []string{"java.lang.String"},
				IsVarargs:      true,
				ExceptionTypes: []string{},
			},
			{
				Name:           "privateMethod",
				Modifiers:      []string{"private"},
				ReturnType:     "void",
				ParameterTypes: []string{},
				ExceptionTypes: []string{},
			},
		},
		Fields: []javareflect.FieldInfo{
			{
				Name:      "VERSION",
				Modifiers: []string{"public", "static", "final"},
				Type:      "java.lang.String",
			},
		},
		Constructors: []javareflect.ConstructorInfo{},
	}
}

func TestSynth(t *testing.T) {
	result := Synth(fixtureClassInfo())
	wc := result.WrapClass
	if wc == nil {
		t.Fatal("WrapClass is nil")
	}
	if wc.ClassName != "MochiWrap_com_example_Greeter" {
		t.Errorf("ClassName = %q; unexpected", wc.ClassName)
	}
	// Should have 2 methods (greet and count; variadics and privateMethod skipped).
	if len(wc.Methods) != 2 {
		t.Errorf("len(Methods) = %d; want 2: %v", len(wc.Methods), wc.Methods)
	}
	// Varargs method should be in skips (private method is just not included).
	if len(result.Skips) < 1 {
		t.Errorf("len(Skips) = %d; want >= 1 (varargs)", len(result.Skips))
	}
}

func TestSynthMethodNames(t *testing.T) {
	result := Synth(fixtureClassInfo())
	names := map[string]bool{}
	for _, m := range result.WrapClass.Methods {
		names[m.WrapName] = true
	}
	// greet is an instance method.
	if !names["MochiWrap_com_example_Greeter_greet"] {
		t.Errorf("missing instance method wrap name, got %v", names)
	}
	// count is static.
	if !names["MochiWrap_com_example_Greeter_static_count"] {
		t.Errorf("missing static method wrap name, got %v", names)
	}
}

func TestSynthFieldMapping(t *testing.T) {
	result := Synth(fixtureClassInfo())
	if len(result.WrapClass.Fields) != 1 {
		t.Fatalf("len(Fields) = %d; want 1", len(result.WrapClass.Fields))
	}
	f := result.WrapClass.Fields[0]
	if f.Mapping.Kind != typemap.KindString {
		t.Errorf("Field mapping Kind = %v; want KindString", f.Mapping.Kind)
	}
}
