package vm

import (
	"testing"

	"github.com/stretchr/testify/require"
	"mochi/interpreter"
)

func TestMBFRoundTrip(t *testing.T) {
	prog := &Program{Funcs: []Function{
		{
			Name:    "main",
			NumRegs: 1,
			Line:    1,
			Code: []Instr{
				{Op: OpConst, A: 0, Val: Value{Tag: interpreter.TagInt, Int: 42}},
				{Op: OpReturn, A: 0},
			},
		},
	}}

	data, err := MarshalMBF(prog)
	require.NoError(t, err)

	out, err := UnmarshalMBF(data)
	require.NoError(t, err)
	require.Equal(t, prog, out)
}
