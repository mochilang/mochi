package types

import (
	"mochi/parser"
	"testing"
)

// Pin the post-MEP 4 P18 contract: SetFunc stores under the exact name
// only. Earlier the binder auto-added a strings.ToLower alias so a
// `SetFunc("MyFunc", ...)` could also be retrieved as `myfunc`. Nothing
// in-tree relied on this lookup form, and the FFI layer is the proper
// owner of any name-mangling policy.
func TestSetFuncStoresExactNameOnly(t *testing.T) {
	env := NewEnv(nil)
	fn := &parser.FunStmt{Name: "MyFunc"}

	env.SetFunc("MyFunc", fn)

	if got, ok := env.GetFunc("MyFunc"); !ok || got != fn {
		t.Fatalf("exact-case lookup failed: got=%v ok=%v", got, ok)
	}
	if _, ok := env.GetFunc("myfunc"); ok {
		t.Fatalf("lowercase alias should no longer be added by SetFunc")
	}
}
