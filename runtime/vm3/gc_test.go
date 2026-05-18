package vm3

import "testing"

// TestPhase5CollectFreesUnreachable verifies the basic invariant: a
// slot allocated and discarded (no live regs or consts referencing
// it) is freed by Collect, while a slot held by a live root survives.
func TestPhase5CollectFreesUnreachable(t *testing.T) {
	vm := New()
	// Allocate two slots; neither is reachable from any root.
	_ = vm.arenas.AllocMap(0)
	_ = vm.arenas.AllocList(0, 0)
	if vm.arenas.LiveSlots(ArenaMap) != 1 {
		t.Fatalf("pre-Collect LiveSlots(ArenaMap) = %d, want 1", vm.arenas.LiveSlots(ArenaMap))
	}
	vm.Collect()
	if got := vm.arenas.LiveSlots(ArenaMap); got != 0 {
		t.Errorf("post-Collect LiveSlots(ArenaMap) = %d, want 0", got)
	}
	if got := vm.arenas.LiveSlots(ArenaList); got != 0 {
		t.Errorf("post-Collect LiveSlots(ArenaList) = %d, want 0", got)
	}
	// TotalSlots stays at 2 (slabs aren't shrunk; free list reuses).
	if got := vm.arenas.TotalSlots(ArenaMap); got != 1 {
		t.Errorf("post-Collect TotalSlots(ArenaMap) = %d, want 1", got)
	}
}

// TestPhase5CollectKeepsRootInStack verifies that a Cell held in
// vm.stackCell counts as a root and survives Collect.
func TestPhase5CollectKeepsRootInStack(t *testing.T) {
	vm := New()
	held := vm.arenas.AllocMap(0)
	_ = vm.arenas.AllocList(0, 0) // unreachable
	// Plant the held handle into stackCell to simulate a live frame.
	vm.stackCell = append(vm.stackCell, held)
	vm.Collect()
	if got := vm.arenas.LiveSlots(ArenaMap); got != 1 {
		t.Errorf("Map slot rooted in stackCell freed: LiveSlots=%d, want 1", got)
	}
	if got := vm.arenas.LiveSlots(ArenaList); got != 0 {
		t.Errorf("unreachable List slot survived: LiveSlots=%d, want 0", got)
	}
}

// TestPhase5CollectFollowsListCells verifies that a list whose cells
// hold a handle into ArenaString causes the string to be marked
// transitively when the list is rooted.
func TestPhase5CollectFollowsListCells(t *testing.T) {
	vm := New()
	listCell := vm.arenas.AllocList(0, 0)
	strCell := vm.arenas.AllocString([]byte("hello"))
	_, _, lidx := listCell.DecodeHandle()
	vm.arenas.Lists[lidx].cells = append(vm.arenas.Lists[lidx].cells, strCell)
	vm.arenas.Lists[lidx].len = 1
	vm.stackCell = append(vm.stackCell, listCell)
	vm.Collect()
	if got := vm.arenas.LiveSlots(ArenaList); got != 1 {
		t.Errorf("rooted list freed: LiveSlots=%d, want 1", got)
	}
	if got := vm.arenas.LiveSlots(ArenaString); got != 1 {
		t.Errorf("string reachable via list freed: LiveSlots=%d, want 1", got)
	}
}

// TestPhase5CollectHandlesCycle verifies that a cycle in the handle
// graph (struct field pointing back to a list containing the struct)
// doesn't loop forever in mark: flagMarked short-circuits.
func TestPhase5CollectHandlesCycle(t *testing.T) {
	vm := New()
	listCell := vm.arenas.AllocList(0, 0)
	structCell := vm.arenas.AllocStruct(0, 1)
	_, _, lidx := listCell.DecodeHandle()
	_, _, sidx := structCell.DecodeHandle()
	// list[0] = struct, struct.fields[0] = list
	vm.arenas.Lists[lidx].cells = append(vm.arenas.Lists[lidx].cells, structCell)
	vm.arenas.Lists[lidx].len = 1
	vm.arenas.Structs[sidx].fields[0] = listCell
	vm.stackCell = append(vm.stackCell, listCell)
	vm.Collect()
	if got := vm.arenas.LiveSlots(ArenaList); got != 1 {
		t.Errorf("rooted list in cycle freed: LiveSlots=%d, want 1", got)
	}
	if got := vm.arenas.LiveSlots(ArenaStruct); got != 1 {
		t.Errorf("struct in cycle freed: LiveSlots=%d, want 1", got)
	}
}

// TestPhase5CollectBumpsGenOnFree verifies that a freed slot's
// generation counter is bumped, so any stale handle to it fails the
// debug-mode gen check.
func TestPhase5CollectBumpsGenOnFree(t *testing.T) {
	vm := New()
	c := vm.arenas.AllocMap(0)
	_, gen, idx := c.DecodeHandle()
	if gen != 0 {
		t.Fatalf("initial gen = %d, want 0", gen)
	}
	vm.Collect()
	got := vm.arenas.Maps[idx].gen
	if got != 1 {
		t.Errorf("post-Collect Maps[%d].gen = %d, want 1", idx, got)
	}
}

// TestPhase5BoundsReusedVMRuns asserts that calling Collect between
// reused-VM runs of a list-returning kernel keeps memory bounded:
// after 1000 runs with Collect between each, TotalSlots(ArenaList)
// is at most 2 (one survivor + free-list reuse churn), not 1000.
func TestPhase5BoundsReusedVMRuns(t *testing.T) {
	main := &Function{
		Name:        "returnFreshList",
		NumRegsI64:  1,
		NumRegsCell: 1,
		ResultBank:  BankCell,
		Code: []Op{
			MakeOp(OpNewList, 0, 0, 0),
			MakeOp(OpConstI64K, 0, 0, 7),
			MakeOp(OpListPushI64, 0, 0, 0),
			MakeOp(OpReturnCell, 0, 0, 0),
		},
	}
	prog := &Program{Funcs: []*Function{main}, Entry: 0}
	vm := NewWithProgram(prog)
	const N = 1000
	for range N {
		if _, err := vm.Run(main); err != nil {
			t.Fatalf("Run error: %v", err)
		}
		vm.Collect()
	}
	if got := vm.arenas.TotalSlots(ArenaList); got > 2 {
		t.Errorf("TotalSlots(ArenaList) after %d runs+Collects = %d, want <=2", N, got)
	}
	if got := vm.arenas.LiveSlots(ArenaList); got != 0 {
		t.Errorf("LiveSlots(ArenaList) after final Collect = %d, want 0", got)
	}
}
