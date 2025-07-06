--- FAIL: TestLuaCompiler_GoldenOutput (0.02s)
    --- FAIL: TestLuaCompiler_GoldenOutput/abs_builtin (0.00s)
        golden.go:70: process error: ❌ type error: error[T003]: unknown function: abs
              --> /workspace/mochi/tests/compiler/lua/abs_builtin.mochi:1:7
            
              1 | print(abs(-3))
                |       ^
            
            help:
              Ensure the function is defined before it's called.
FAIL
FAIL	mochi/compile/x/lua	0.059s
FAIL
