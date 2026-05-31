package asyncbridge_test

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"mochi/package3/dotnet/asyncbridge"
	"mochi/package3/dotnet/typemap"
)

// TestPhase11AsyncBridge is the MEP-68 Phase 11 gate for the async bridge.
//
// It verifies:
//   - WrapAsync returns a MethodSpec with "_Sync" suffix and IsAsync=false.
//   - EmitAsyncWrapperCS emits valid C# containing ManualResetEventSlim and [UnmanagedCallersOnly].
func TestPhase11AsyncBridge(t *testing.T) {
	t.Run("WrapAsync", func(t *testing.T) {
		m := asyncbridge.MethodSpec{
			Name:       "GetUserAge",
			IsAsync:    true,
			ReturnKind: typemap.KindInt64,
			ReturnType: "TaskOfInt64",
			ParamNames: []string{"namePtr"},
		}
		wrapped := asyncbridge.WrapAsync(m)
		if wrapped.Name != "GetUserAge_Sync" {
			t.Errorf("Name: got %q, want GetUserAge_Sync", wrapped.Name)
		}
		if wrapped.IsAsync {
			t.Error("WrapAsync result should have IsAsync=false")
		}
		if wrapped.ReturnKind != typemap.KindInt64 {
			t.Errorf("ReturnKind: got %v, want KindInt64", wrapped.ReturnKind)
		}
	})

	t.Run("EmitAsyncWrapperCS", func(t *testing.T) {
		methods := []asyncbridge.MethodSpec{
			{
				Name:             "GetUserAge",
				IsAsync:          true,
				ReturnKind:       typemap.KindInt64,
				ReturnType:       "TaskOfInt64",
				ParamNames:       []string{"namePtr"},
				ParamCSharpTypes: []string{"nint"},
			},
			{
				Name:       "NotAsync",
				IsAsync:    false,
				ReturnKind: typemap.KindString,
			},
		}

		outDir := t.TempDir()
		outPath := filepath.Join(outDir, "AsyncWrapper.cs")
		if err := asyncbridge.EmitAsyncWrapperCS(methods, outPath); err != nil {
			t.Fatalf("EmitAsyncWrapperCS: %v", err)
		}

		data, err := os.ReadFile(outPath)
		if err != nil {
			t.Fatalf("ReadFile: %v", err)
		}
		cs := string(data)

		if !strings.Contains(cs, "ManualResetEventSlim") {
			t.Error("AsyncWrapper.cs missing ManualResetEventSlim")
		}
		if !strings.Contains(cs, "[UnmanagedCallersOnly") {
			t.Error("AsyncWrapper.cs missing [UnmanagedCallersOnly")
		}
		if !strings.Contains(cs, "GetUserAge") {
			t.Error("AsyncWrapper.cs missing GetUserAge method")
		}
		// Sync-only method should not appear (EmitAsyncWrapperCS only emits async methods).
		if strings.Contains(cs, "NotAsync") {
			t.Error("AsyncWrapper.cs should not contain NotAsync (sync method)")
		}
	})
}
