package types

import "testing"

// Pin closure-style semantics for Env.Copy. Pre-MEP 4 P17 this method
// detached the parent chain, so a closure created inside a function body
// would silently lose bindings from enclosing scopes. The fix is to keep
// the parent pointer; the local maps are still cloned so that writes to
// the copy do not leak back into the original.
func TestEnvCopyPreservesParentChain(t *testing.T) {
	root := NewEnv(nil)
	root.SetValue("outer", 100, false)

	mid := NewEnv(root)
	mid.SetValue("inner", 10, true)

	cp := mid.Copy()

	if v, err := cp.GetValue("inner"); err != nil || v != 10 {
		t.Fatalf("local value lost: v=%v err=%v", v, err)
	}
	if v, err := cp.GetValue("outer"); err != nil || v != 100 {
		t.Fatalf("outer value not visible through preserved parent: v=%v err=%v", v, err)
	}

	cp.SetValue("inner", 99, true)
	if v, err := mid.GetValue("inner"); err != nil || v != 10 {
		t.Fatalf("mutation on copy leaked into original: v=%v err=%v", v, err)
	}
}
