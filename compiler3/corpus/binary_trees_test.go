package corpus

import (
	"testing"

	vm3 "mochi/runtime/vm3"
)

// TestBinaryTreesMatchesOracle runs the vm3 binary_trees kernel through
// the interpreter and asserts strict equality against
// ExpectBinaryTrees. The depth sweep covers the leaf case (0), small
// depths (1..4), and one mid-size depth (8) so the recursive pair
// arena alloc / PairFst / PairSnd path is exercised end-to-end without
// triggering the slow BG bench sizes (depth=10/12 are saved for the
// bench harness).
func TestBinaryTreesMatchesOracle(t *testing.T) {
	for _, depth := range []int64{0, 1, 2, 3, 4, 5, 8} {
		prog := BinaryTrees.Build(depth)
		vm := vm3.NewWithProgram(prog)
		got, err := vm.RunWithArgs(prog.Funcs[prog.Entry], []int64{depth})
		if err != nil {
			t.Fatalf("binary_trees(%d): %v", depth, err)
		}
		want := ExpectBinaryTrees(depth)
		if got.Int() != want {
			t.Errorf("binary_trees(%d) = %d, want %d", depth, got.Int(), want)
		}
	}
}

// BenchmarkBinaryTreesInterp measures interp-only throughput on the
// two BG cross-lang sizes.
func BenchmarkBinaryTreesInterp(b *testing.B) {
	for _, tc := range []struct {
		name  string
		depth int64
	}{
		{"binary_trees_n10", 10},
		{"binary_trees_n12", 12},
	} {
		b.Run(tc.name, func(b *testing.B) {
			prog := BinaryTrees.Build(tc.depth)
			vm := vm3.NewWithProgram(prog)
			fn := prog.Funcs[prog.Entry]
			args := []int64{tc.depth}
			b.ResetTimer()
			for range b.N {
				_, _ = vm.RunWithArgs(fn, args)
			}
		})
	}
}

type goTree []goTree

func goMakeTree(d int64) goTree {
	if d == 0 {
		return goTree{}
	}
	return goTree{goMakeTree(d - 1), goMakeTree(d - 1)}
}

func goCheckTree(t goTree) int64 {
	if len(t) == 0 {
		return 1
	}
	return 1 + goCheckTree(t[0]) + goCheckTree(t[1])
}

func goBinaryTrees(depth int64) int64 {
	iters := int64(1)
	for range depth {
		iters *= 2
	}
	var total int64
	for range iters {
		total += goCheckTree(goMakeTree(depth))
	}
	return total
}

var binaryTreesGoSink int64

// BenchmarkBinaryTreesGo is the matching native-Go reference (mirrors
// bench/template/bg/binary_trees/binary_trees.go.tmpl: actually
// allocates and walks the nested-2-slice tree) so the vm3-vs-Go ratio
// is fair.
func BenchmarkBinaryTreesGo(b *testing.B) {
	for _, tc := range []struct {
		name  string
		depth int64
	}{
		{"binary_trees_n10", 10},
		{"binary_trees_n12", 12},
	} {
		b.Run(tc.name, func(b *testing.B) {
			d := tc.depth
			var s int64
			for i := 0; i < b.N; i++ {
				s += goBinaryTrees(d + int64(i&1))
			}
			binaryTreesGoSink = s
		})
	}
}
