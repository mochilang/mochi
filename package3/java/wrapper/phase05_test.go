package wrapper

import (
	"strings"
	"testing"

	javareflect "mochi/package3/java/reflect"
)

// TestPhase5WrapperSynth is the phase-gate sentinel test for MEP-67 phase 5.
func TestPhase5WrapperSynth(t *testing.T) {
	surface := &javareflect.Surface{
		SchemaVersion: 1,
		Classes: []javareflect.ClassInfo{
			{
				Name:      "com.example.Calculator",
				Modifiers: []string{"public"},
				Methods: []javareflect.MethodInfo{
					{
						Name:           "add",
						Modifiers:      []string{"public", "static"},
						ReturnType:     "int",
						ParameterTypes: []string{"int", "int"},
						ParameterNames: []string{"a", "b"},
						ExceptionTypes: []string{},
					},
					{
						Name:           "toString",
						Modifiers:      []string{"public"},
						ReturnType:     "java.lang.String",
						ParameterTypes: []string{},
						ExceptionTypes: []string{},
					},
				},
				Fields: []javareflect.FieldInfo{
					{
						Name:      "PI",
						Modifiers: []string{"public", "static", "final"},
						Type:      "double",
					},
				},
				Constructors: []javareflect.ConstructorInfo{},
			},
			{
				Name:      "com.example.Helper",
				Modifiers: []string{"public"},
				Methods: []javareflect.MethodInfo{
					{
						Name:           "format",
						Modifiers:      []string{"public"},
						ReturnType:     "java.lang.String",
						ParameterTypes: []string{"java.lang.String", "int"},
						ParameterNames: []string{"template", "value"},
						ExceptionTypes: []string{},
					},
				},
				Fields:       []javareflect.FieldInfo{},
				Constructors: []javareflect.ConstructorInfo{},
			},
		},
	}

	t.Run("synth_two_classes", func(t *testing.T) {
		for _, cls := range surface.Classes {
			result := Synth(cls)
			if result.WrapClass == nil {
				t.Errorf("Synth(%s) returned nil WrapClass", cls.Name)
			}
		}
	})

	t.Run("method_names", func(t *testing.T) {
		result := Synth(surface.Classes[0])
		names := map[string]bool{}
		for _, m := range result.WrapClass.Methods {
			names[m.WrapName] = true
		}
		if !names["MochiWrap_com_example_Calculator_static_add"] {
			t.Errorf("missing static add method, got %v", names)
		}
		if !names["MochiWrap_com_example_Calculator_toString"] {
			t.Errorf("missing toString method, got %v", names)
		}
	})

	t.Run("emit_java_source", func(t *testing.T) {
		result := Synth(surface.Classes[0])
		src := EmitJavaSource(result.WrapClass)
		wantBits := []string{
			"public class MochiWrap_com_example_Calculator",
			"MochiWrap_com_example_Calculator_static_add",
			"int a",
			"int b",
		}
		for _, bit := range wantBits {
			if !strings.Contains(src, bit) {
				t.Errorf("EmitJavaSource missing %q\n--- output ---\n%s", bit, src)
			}
		}
	})
}
