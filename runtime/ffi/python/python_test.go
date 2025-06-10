package python_test

import (
	"testing"

	"mochi/runtime/ffi/python"
)

func TestCallStdlib(t *testing.T) {
	res, err := python.Call("math", "sqrt", 9)
	if err != nil {
		t.Fatalf("call math.sqrt: %v", err)
	}
	f, ok := res.(float64)
	if !ok || f != 3 {
		t.Fatalf("unexpected result: %v", res)
	}
}

func TestExecAdd(t *testing.T) {
	res, err := python.Exec(`
return args[0] + args[1]
`, 2, 3)
	if err != nil {
		t.Fatalf("exec: %v", err)
	}
	n, ok := res.(float64)
	if !ok || n != 5 {
		t.Fatalf("unexpected result: %v", res)
	}
}
