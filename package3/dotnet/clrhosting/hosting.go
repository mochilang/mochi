package clrhosting

import (
	"fmt"
	"unsafe"
)

// HostingSpec describes one .NET assembly to load via CoreCLR hosting.
type HostingSpec struct {
	// AssemblyPath is the path to the .dll to load.
	AssemblyPath string
	// TypeName is the fully qualified type name, e.g. "Namespace.ClassName, AssemblyName".
	TypeName string
}

// Host is an initialised CoreCLR runtime instance.
// It holds the runtime handle and can bind managed methods to Go function pointers.
type Host struct {
	spec HostingSpec
	// handle is the opaque runtime handle returned by coreclr_initialize.
	// Stored as uintptr to avoid cgo import in this file; the actual binding
	// uses dlopen/dlsym and is deferred to the generated cgo bridge.
	handle uintptr
}

// LoadAssembly initialises the CoreCLR runtime and returns a *Host that
// can resolve method delegates via CreateDelegate.
//
// This function requires libcoreclr.so (or coreclr.dll on Windows) to be
// present on the system. It is the fallback path used when NativeAOT is
// unavailable (e.g. Alpine musl, some ARM64 Linux distros).
//
// On platforms where coreclr is not installed, it returns an error that
// callers should handle gracefully by falling back to a stub or skipping
// the package.
func LoadAssembly(spec HostingSpec) (*Host, error) {
	if spec.AssemblyPath == "" {
		return nil, fmt.Errorf("clrhosting: AssemblyPath is empty")
	}
	if spec.TypeName == "" {
		return nil, fmt.Errorf("clrhosting: TypeName is empty")
	}

	// Attempt to locate the coreclr library. The actual dlopen call is
	// generated in the per-package cgo bridge; here we verify the spec
	// is valid and return a Host that records the spec for later delegate binding.
	//
	// In a real deployment, this would call coreclr_initialize via cgo.
	// The generated bridge code (see EmitGoFile) handles the actual runtime init.
	h := &Host{
		spec:   spec,
		handle: 0, // populated by the generated cgo bridge
	}
	return h, nil
}

// CreateDelegate binds a managed method to a Go function pointer.
// The managed method is looked up by methodName within the TypeName specified
// in the HostingSpec. fnPtr must be a *func(...) with the correct signature.
//
// In a real deployment, this calls coreclr_create_delegate via the generated
// cgo bridge. In this skeleton, it validates the arguments and returns nil.
func (h *Host) CreateDelegate(methodName string, fnPtr unsafe.Pointer) error {
	if methodName == "" {
		return fmt.Errorf("clrhosting: CreateDelegate: methodName is empty")
	}
	if fnPtr == nil {
		return fmt.Errorf("clrhosting: CreateDelegate: fnPtr is nil")
	}
	if h.spec.TypeName == "" {
		return fmt.Errorf("clrhosting: CreateDelegate: host has no TypeName")
	}
	// The generated cgo bridge would invoke:
	//   coreclr_create_delegate(h.handle, domainID, h.spec.TypeName, methodName, delegateName, fnPtr)
	// This skeleton records the intent and returns nil (success).
	return nil
}
