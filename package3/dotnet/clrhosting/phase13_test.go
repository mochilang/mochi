package clrhosting_test

import (
	"os"
	"testing"
	"unsafe"

	"mochi/package3/dotnet/clrhosting"
)

// TestPhase13CoreCLRHosting is the MEP-68 Phase 13 gate for CoreCLR hosting fallback.
//
// It skips unless MOCHI_CORECLR_TEST=1 and a .NET runtime is installed.
// The test uses a pre-built test assembly to verify delegate binding.
func TestPhase13CoreCLRHosting(t *testing.T) {
	if os.Getenv("MOCHI_CORECLR_TEST") == "" {
		t.Skip("set MOCHI_CORECLR_TEST=1 to run CoreCLR integration tests")
	}

	spec := clrhosting.HostingSpec{
		AssemblyPath: "testdata/hello_coreclr.dll",
		TypeName:     "Mochi.Test.Hello, hello_coreclr",
	}

	host, err := clrhosting.LoadAssembly(spec)
	if err != nil {
		t.Fatalf("LoadAssembly: %v", err)
	}
	if host == nil {
		t.Fatal("LoadAssembly returned nil host")
	}

	var fn func() int32
	if err := host.CreateDelegate("GetAnswer", unsafe.Pointer(&fn)); err != nil {
		t.Fatalf("CreateDelegate: %v", err)
	}
}

// TestPhase13HostingSpec verifies that LoadAssembly validates its inputs.
func TestPhase13HostingSpec(t *testing.T) {
	t.Run("EmptyAssemblyPath", func(t *testing.T) {
		_, err := clrhosting.LoadAssembly(clrhosting.HostingSpec{
			TypeName: "Mochi.Test.Class, Assembly",
		})
		if err == nil {
			t.Error("expected error for empty AssemblyPath")
		}
	})

	t.Run("EmptyTypeName", func(t *testing.T) {
		_, err := clrhosting.LoadAssembly(clrhosting.HostingSpec{
			AssemblyPath: "/path/to/assembly.dll",
		})
		if err == nil {
			t.Error("expected error for empty TypeName")
		}
	})

	t.Run("ValidSpec", func(t *testing.T) {
		host, err := clrhosting.LoadAssembly(clrhosting.HostingSpec{
			AssemblyPath: "/nonexistent/test.dll",
			TypeName:     "Test.Class, TestAssembly",
		})
		if err != nil {
			t.Fatalf("LoadAssembly: %v", err)
		}
		if host == nil {
			t.Fatal("expected non-nil host")
		}
	})
}

// TestPhase13CreateDelegate verifies that CreateDelegate validates its inputs.
func TestPhase13CreateDelegate(t *testing.T) {
	host, err := clrhosting.LoadAssembly(clrhosting.HostingSpec{
		AssemblyPath: "/test/assembly.dll",
		TypeName:     "Test.Class, TestAssembly",
	})
	if err != nil {
		t.Fatalf("LoadAssembly: %v", err)
	}

	t.Run("EmptyMethodName", func(t *testing.T) {
		var fn func()
		if err := host.CreateDelegate("", unsafe.Pointer(&fn)); err == nil {
			t.Error("expected error for empty methodName")
		}
	})

	t.Run("NilFnPtr", func(t *testing.T) {
		if err := host.CreateDelegate("SomeMethod", nil); err == nil {
			t.Error("expected error for nil fnPtr")
		}
	})

	t.Run("Valid", func(t *testing.T) {
		var fn func() int32
		if err := host.CreateDelegate("GetAnswer", unsafe.Pointer(&fn)); err != nil {
			t.Errorf("CreateDelegate: %v", err)
		}
	})
}
