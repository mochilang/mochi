package asyncbridge

import (
	"fmt"
	"strings"
)

// AsyncFn is the descriptor for a single imported `async def` callable.
// The wrapper synthesiser (Phase 5) hands one of these to the bridge for
// every async function in the package surface, and the bridge emits the
// corresponding sync shim via RenderShim.
//
// Name is the Python attribute on the imported module ("fetch"). SyncName
// is the synthesised shim identifier ("fetch_sync"); when empty,
// DefaultSyncName supplies the conventional `<name>_sync`. ParamNames and
// ParamTypes parallel-track the parameter list in declaration order;
// ParamTypes carries the Python type annotation (already resolved by the
// typemap layer, so "list[int]" not "List[int]"). Return is the Python
// return type, which the shim uses as the unwrap target after the loop
// drives the coroutine to completion.
type AsyncFn struct {
	Name       string
	SyncName   string
	ParamNames []string
	ParamTypes []string
	Return     string
}

// Validate reports the structural defects that would cause RenderShim to
// produce nonsense Python. It is deliberately strict about the (names,
// types) pairing because the wrapper synthesiser builds both lists from
// the same .pyi traversal; a mismatch indicates an upstream bug.
func (f AsyncFn) Validate() error {
	if strings.TrimSpace(f.Name) == "" {
		return fmt.Errorf("asyncbridge: AsyncFn.Name is empty")
	}
	if strings.TrimSpace(f.SyncName) == "" {
		return fmt.Errorf("asyncbridge: AsyncFn.SyncName is empty for %q", f.Name)
	}
	if f.Name == f.SyncName {
		return fmt.Errorf("asyncbridge: AsyncFn.SyncName must differ from Name (%q)", f.Name)
	}
	if len(f.ParamNames) != len(f.ParamTypes) {
		return fmt.Errorf("asyncbridge: AsyncFn %q has %d param names but %d param types", f.Name, len(f.ParamNames), len(f.ParamTypes))
	}
	for i, n := range f.ParamNames {
		if strings.TrimSpace(n) == "" {
			return fmt.Errorf("asyncbridge: AsyncFn %q param %d has empty name", f.Name, i)
		}
		if strings.TrimSpace(f.ParamTypes[i]) == "" {
			return fmt.Errorf("asyncbridge: AsyncFn %q param %q has empty type", f.Name, n)
		}
	}
	if strings.TrimSpace(f.Return) == "" {
		return fmt.Errorf("asyncbridge: AsyncFn %q has empty Return", f.Name)
	}
	return nil
}

// DefaultSyncName returns the conventional `<name>_sync` shim identifier.
// Callers may override by setting AsyncFn.SyncName directly; this helper
// is exposed so the wrapper synthesiser and the bridge agree on the
// fallback shape.
func DefaultSyncName(name string) string {
	return name + "_sync"
}
