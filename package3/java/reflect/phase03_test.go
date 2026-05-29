package reflect

import (
	"context"
	"errors"
	"fmt"
	"testing"
)

// TestPhase3ReflectTool is the phase-gate sentinel test for MEP-67 phase 3.
// It verifies the reflection tool, surface parsing, and mock executor work correctly.
func TestPhase3ReflectTool(t *testing.T) {
	t.Run("parse_surface", func(t *testing.T) {
		s, err := ParseSurface([]byte(fixtureSurfaceJSON))
		if err != nil {
			t.Fatalf("ParseSurface: %v", err)
		}
		if s.SchemaVersion != 1 {
			t.Errorf("SchemaVersion = %d; want 1", s.SchemaVersion)
		}
		if len(s.Classes) == 0 {
			t.Error("Classes is empty")
		}
	})

	t.Run("class_and_methods", func(t *testing.T) {
		tool := NewToolWithExecutor(mockExecutor(fixtureSurfaceJSON))
		s, err := tool.Invoke(context.Background(), "dummy.jar")
		if err != nil {
			t.Fatalf("Invoke: %v", err)
		}
		if len(s.Classes) != 1 {
			t.Fatalf("len(Classes) = %d; want 1", len(s.Classes))
		}
		cls := s.Classes[0]
		if cls.Name != "com.google.common.base.Optional" {
			t.Errorf("Class.Name = %q; unexpected", cls.Name)
		}
		methodNames := make(map[string]bool)
		for _, m := range cls.Methods {
			methodNames[m.Name] = true
		}
		for _, want := range []string{"of", "isPresent"} {
			if !methodNames[want] {
				t.Errorf("missing method %q", want)
			}
		}
	})

	t.Run("embedded_jar_valid", func(t *testing.T) {
		if len(reflectToolJAR) == 0 {
			t.Fatal("embedded JAR is empty")
		}
		if reflectToolJAR[0] != 0x50 || reflectToolJAR[1] != 0x4B {
			t.Error("embedded JAR missing ZIP magic number")
		}
	})

	t.Run("executor_error_propagated", func(t *testing.T) {
		errFail := errors.New("java not found")
		ex := func(_ context.Context, _ string, _ ...string) ([]byte, error) {
			return nil, errFail
		}
		tool := NewToolWithExecutor(ex)
		_, err := tool.Invoke(context.Background(), "test.jar")
		if err == nil {
			t.Error("expected error from executor")
		}
		if !errors.Is(err, errFail) {
			t.Errorf("error = %v; want to wrap %v", err, errFail)
		}
	})

	t.Run("field_modifiers", func(t *testing.T) {
		tool := NewToolWithExecutor(mockExecutor(fixtureSurfaceJSON))
		s, _ := tool.Invoke(context.Background(), "dummy.jar")
		if len(s.Classes) == 0 {
			t.Skip("no classes")
		}
		if len(s.Classes[0].Fields) == 0 {
			t.Skip("no fields")
		}
		field := s.Classes[0].Fields[0]
		if field.Name != "EMPTY" {
			t.Errorf("Fields[0].Name = %q; want EMPTY", field.Name)
		}
		foundStatic := false
		for _, mod := range field.Modifiers {
			if mod == "static" {
				foundStatic = true
			}
		}
		if !foundStatic {
			t.Errorf("EMPTY field should be static, modifiers: %v", field.Modifiers)
		}
		fmt.Println("field", field.Name, field.Modifiers)
	})
}
