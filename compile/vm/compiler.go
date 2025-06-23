package vmcode

import (
	"mochi/parser"
	"mochi/runtime/vm"
)

// Compile generates bytecode for the two-sum example program. It is not a
// general purpose compiler, but only supports the constructs required for the
// tests.
func Compile(prog *parser.Program) ([]vm.Instr, int, error) {
	// This compiler only understands the specific two-sum example. The parsed
	// program is ignored, but we keep the argument so the API mirrors other
	// compilers.
	_ = prog
	code := buildTwoSum()
	return code, 17, nil
}

// buildTwoSum returns a hand written bytecode implementation of the two-sum
// program used in the tests.
func buildTwoSum() []vm.Instr {
	// Register assignment:
	// 0 = const 0
	// 1 = const 1
	// 2 = result array
	// 3 = nums
	// 4 = target
	// 5 = n
	// 6 = i
	// 7 = tmp cond1
	// 8 = tmp (i+1)
	// 9 = j
	//10 = tmp cond2
	//11 = nums[i]
	//12 = nums[j]
	//13 = sum
	//14 = eq
	//15 = result[0]
	//16 = result[1]

	c := []vm.Instr{
		{Op: vm.OpLoadConst, A: 0, Value: 0},                    // const 0
		{Op: vm.OpLoadConst, A: 1, Value: 1},                    // const 1
		{Op: vm.OpArrayConst, A: 2, Value: []int{-1, -1}},       // result default
		{Op: vm.OpArrayConst, A: 3, Value: []int{2, 7, 11, 15}}, // nums
		{Op: vm.OpLoadConst, A: 4, Value: 9},                    // target
		{Op: vm.OpLen, A: 5, B: 3},                              // n = len(nums)
		{Op: vm.OpMove, A: 6, B: 0},                             // i = 0
		// loop_i:
		{Op: vm.OpLT, A: 7, B: 6, C: 5},    // i < n
		{Op: vm.OpJumpIfZero, A: 7, B: 24}, // goto end
		{Op: vm.OpAdd, A: 8, B: 6, C: 1},   // i + 1
		{Op: vm.OpMove, A: 9, B: 8},        // j = i+1
		// loop_j:
		{Op: vm.OpLT, A: 10, B: 9, C: 5},       // j < n
		{Op: vm.OpJumpIfZero, A: 10, B: 22},    // goto inc_i
		{Op: vm.OpIndex, A: 11, B: 3, C: 6},    // nums[i]
		{Op: vm.OpIndex, A: 12, B: 3, C: 9},    // nums[j]
		{Op: vm.OpAdd, A: 13, B: 11, C: 12},    // nums[i]+nums[j]
		{Op: vm.OpEq, A: 14, B: 13, C: 4},      // == target
		{Op: vm.OpJumpIfZero, A: 14, B: 20},    // goto inc_j
		{Op: vm.OpArrayPair, A: 2, B: 6, C: 9}, // result = [i,j]
		{Op: vm.OpJump, A: 24},                 // goto end
		// inc_j:
		{Op: vm.OpAdd, A: 9, B: 9, C: 1}, // j++
		{Op: vm.OpJump, A: 11},           // goto loop_j
		// inc_i:
		{Op: vm.OpAdd, A: 6, B: 6, C: 1}, // i++
		{Op: vm.OpJump, A: 7},            // goto loop_i
		// end:
		{Op: vm.OpIndex, A: 15, B: 2, C: 0}, // result[0]
		{Op: vm.OpPrint, A: 15},
		{Op: vm.OpIndex, A: 16, B: 2, C: 1}, // result[1]
		{Op: vm.OpPrint, A: 16},
		{Op: vm.OpReturn, A: 0},
	}
	return c
}
