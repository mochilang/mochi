package emit

import (
	"strings"
	"testing"

	javaerrors "mochi/package3/java/errors"
	"mochi/package3/java/maven"
	javareflect "mochi/package3/java/reflect"
	"mochi/package3/java/wrapper"
)

// TestPhase6ExternEmitter is the phase-gate sentinel test for MEP-67 phase 6.
func TestPhase6ExternEmitter(t *testing.T) {
	coord := maven.Coordinate{GroupID: "com.google.guava", ArtifactID: "guava", Version: "33.4.8-jre"}
	surface := &javareflect.Surface{
		SchemaVersion: 1,
		Classes: []javareflect.ClassInfo{
			{
				Name:      "com.google.common.base.Optional",
				Modifiers: []string{"public", "abstract"},
				Methods: []javareflect.MethodInfo{
					{
						Name:           "of",
						Modifiers:      []string{"public", "static"},
						ReturnType:     "com.google.common.base.Optional",
						ParameterTypes: []string{"java.lang.Object"},
						ParameterNames: []string{"reference"},
						ExceptionTypes: []string{},
					},
					{
						Name:           "isPresent",
						Modifiers:      []string{"public", "abstract"},
						ReturnType:     "boolean",
						ParameterTypes: []string{},
						ExceptionTypes: []string{},
					},
				},
				Fields:       []javareflect.FieldInfo{},
				Constructors: []javareflect.ConstructorInfo{},
			},
		},
	}

	t.Run("shim_has_extern_type", func(t *testing.T) {
		shim := EmitShimMochi(surface, coord)
		if !strings.Contains(shim, "extern java type Optional") {
			t.Errorf("shim missing extern type declaration:\n%s", shim)
		}
	})

	t.Run("shim_has_extern_fn", func(t *testing.T) {
		shim := EmitShimMochi(surface, coord)
		if !strings.Contains(shim, "extern java fn") {
			t.Errorf("shim missing extern fn declarations:\n%s", shim)
		}
	})

	t.Run("shim_header", func(t *testing.T) {
		shim := EmitShimMochi(surface, coord)
		if !strings.Contains(shim, "com.google.guava:guava:33.4.8-jre") {
			t.Errorf("shim missing source coordinate:\n%s", shim)
		}
	})

	t.Run("skip_report_nonempty", func(t *testing.T) {
		skips := []javaerrors.SkipReport{
			{ItemPath: "com.google.common.base.Optional.of", Reason: javaerrors.SkipRawObject, Detail: "Object param"},
		}
		report := EmitSkipReport(skips)
		if !strings.Contains(report, "SKIPPED") {
			t.Errorf("skip report should contain SKIPPED header: %s", report)
		}
	})

	t.Run("collect_skips_from_varargs", func(t *testing.T) {
		cls := javareflect.ClassInfo{
			Name: "com.example.Foo",
			Methods: []javareflect.MethodInfo{
				{
					Name:           "variadicMethod",
					Modifiers:      []string{"public"},
					ReturnType:     "void",
					ParameterTypes: []string{"java.lang.String"},
					IsVarargs:      true,
					ExceptionTypes: []string{},
				},
			},
			Fields:       []javareflect.FieldInfo{},
			Constructors: []javareflect.ConstructorInfo{},
		}
		result := wrapper.Synth(cls)
		reports := CollectSkips(cls, result)
		if len(reports) == 0 {
			t.Error("expected at least 1 skip for varargs method")
		}
	})
}
