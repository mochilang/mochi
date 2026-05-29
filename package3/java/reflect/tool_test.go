package reflect

import (
	"context"
	"testing"
)

func mockExecutor(fixtureJSON string) Executor {
	return func(_ context.Context, _ string, _ ...string) ([]byte, error) {
		return []byte(fixtureJSON), nil
	}
}

func TestToolInvokeWithMock(t *testing.T) {
	tool := NewToolWithExecutor(mockExecutor(fixtureSurfaceJSON))
	s, err := tool.Invoke(context.Background(), "/fake/path/test.jar")
	if err != nil {
		t.Fatalf("Invoke: %v", err)
	}
	if len(s.Classes) != 1 {
		t.Errorf("len(Classes) = %d; want 1", len(s.Classes))
	}
	if s.Classes[0].Name != "com.google.common.base.Optional" {
		t.Errorf("Class.Name = %q; unexpected", s.Classes[0].Name)
	}
}

func TestToolInvokeMethodCount(t *testing.T) {
	tool := NewToolWithExecutor(mockExecutor(fixtureSurfaceJSON))
	s, err := tool.Invoke(context.Background(), "test.jar")
	if err != nil {
		t.Fatalf("Invoke: %v", err)
	}
	cls := s.Classes[0]
	if len(cls.Methods) != 2 {
		t.Errorf("len(Methods) = %d; want 2", len(cls.Methods))
	}
	if cls.Methods[0].Name != "of" {
		t.Errorf("Methods[0].Name = %q; want of", cls.Methods[0].Name)
	}
}

func TestEmbeddedJAR(t *testing.T) {
	// Verify the embedded JAR is a valid ZIP (JAR) file.
	if len(reflectToolJAR) == 0 {
		t.Fatal("embedded reflectToolJAR is empty")
	}
	// ZIP magic number: PK (0x50 0x4B).
	if reflectToolJAR[0] != 0x50 || reflectToolJAR[1] != 0x4B {
		t.Errorf("embedded JAR does not start with ZIP magic: %x %x", reflectToolJAR[0], reflectToolJAR[1])
	}
}
