--- FAIL: TestFSCompiler_GoldenOutput (154.55s)
    --- FAIL: TestFSCompiler_GoldenOutput/avg_builtin (1.38s)
        golden.go:70: process error: ❌ fsi error: exit status 1
            
            
            /tmp/TestFSCompiler_GoldenOutput2649731433/037/main.fsx(3,37): error FS0001: The type 'int' does not support the operator 'DivideByInt'
            
    --- FAIL: TestFSCompiler_GoldenOutput/break_continue#01 (1.83s)
        golden.go:70: process error: runtime mismatch
            
            --- F# ---
            ("odd number:", 1)
            ("odd number:", 3)
            ("odd number:", 5)
            ("odd number:", 7)
            
            --- VM ---
            odd number: 1
            odd number: 3
            odd number: 5
            odd number: 7
    --- FAIL: TestFSCompiler_GoldenOutput/ceil_builtin (1.77s)
        golden.go:70: process error: runtime mismatch
            
            --- F# ---
            3.0
            
            --- VM ---
            <nil>
    --- FAIL: TestFSCompiler_GoldenOutput/dataset (1.50s)
        golden.go:70: process error: ❌ fsi error: exit status 1
            
            
            /tmp/TestFSCompiler_GoldenOutput2649731433/043/main.fsx(7,51): error FS0001: This expression was expected to have type
                'string'    
            but here has type
                'int'    
            
    --- FAIL: TestFSCompiler_GoldenOutput/dataset_sort_take_limit (2.04s)
        golden.go:70: process error: runtime mismatch
            
            --- F# ---
            "--- Top products (excluding most expensive) ---"
            ("Smartphone", "costs $", 900)
            ("Tablet", "costs $", 600)
            ("Monitor", "costs $", 300)
            
            --- VM ---
            --- Top products (excluding most expensive) ---
            Smartphone costs $ 900
            Tablet costs $ 600
            Monitor costs $ 300
    --- FAIL: TestFSCompiler_GoldenOutput/expect_simple (1.79s)
        golden.go:70: process error: runtime mismatch
            
            --- F# ---
            "ok"
            
            --- VM ---
            ok
    --- FAIL: TestFSCompiler_GoldenOutput/fetch_builtin (1.90s)
        golden.go:70: process error: ❌ fsi error: exit status 1
            
            
            /tmp/TestFSCompiler_GoldenOutput2649731433/046/main.fsx(45,33): error FS0039: The type 'HttpResponseMessage' does not define the field, constructor or member 'Result'.
            
    --- FAIL: TestFSCompiler_GoldenOutput/fetch_cast (2.10s)
        golden.go:70: process error: ❌ fsi error: exit status 1
            
            
            /tmp/TestFSCompiler_GoldenOutput2649731433/047/main.fsx(51,33): error FS0039: The type 'HttpResponseMessage' does not define the field, constructor or member 'Result'.
            
    --- FAIL: TestFSCompiler_GoldenOutput/fetch_http (1.75s)
        golden.go:70: process error: ❌ fsi error: exit status 1
            
            
            /tmp/TestFSCompiler_GoldenOutput2649731433/048/main.fsx(45,33): error FS0039: The type 'HttpResponseMessage' does not define the field, constructor or member 'Result'.
            
    --- FAIL: TestFSCompiler_GoldenOutput/float_literal_precision (1.67s)
        golden.go:70: process error: runtime mismatch
            
            --- F# ---
            9.261
            
            --- VM ---
            9.261000000000001
    --- FAIL: TestFSCompiler_GoldenOutput/floor_builtin (1.98s)
        golden.go:70: process error: runtime mismatch
            
            --- F# ---
            2.0
            
            --- VM ---
            <nil>
    --- FAIL: TestFSCompiler_GoldenOutput/for_string_collection#01 (1.70s)
        golden.go:70: process error: runtime mismatch
            
            --- F# ---
            "h"
            "i"
            
            --- VM ---
            h
            i
    --- FAIL: TestFSCompiler_GoldenOutput/group_by (1.84s)
        golden.go:70: process error: ❌ fsi error: exit status 1
            
            
            /tmp/TestFSCompiler_GoldenOutput2649731433/056/main.fsx(20,24): error FS0001: This expression was expected to have type
                ''a list'    
            but here has type
                'int array'    
            
    --- FAIL: TestFSCompiler_GoldenOutput/if_else#01 (1.76s)
        golden.go:70: process error: runtime mismatch
            
            --- F# ---
            -1
            0
            1
            
            --- VM ---
            -1
            0
            3
    --- FAIL: TestFSCompiler_GoldenOutput/list_concat (1.95s)
        golden.go:70: process error: runtime mismatch
            
            --- F# ---
            [|1; 2; 3; 4|]
            
            --- VM ---
            0
    --- FAIL: TestFSCompiler_GoldenOutput/list_prepend (1.77s)
        golden.go:70: process error: runtime mismatch
            
            --- F# ---
            [|[|1; 2|]; [|3|]; [|4|]|]
            
            --- VM ---
            0
    --- FAIL: TestFSCompiler_GoldenOutput/list_slice (1.96s)
        golden.go:70: process error: runtime mismatch
            
            --- F# ---
            [|2; 3|]
            
            --- VM ---
            2 3
    --- FAIL: TestFSCompiler_GoldenOutput/load_save_json (1.76s)
        golden.go:70: process error: ❌ fsi error: exit status 1
            
            
            /tmp/TestFSCompiler_GoldenOutput2649731433/064/main.fsx(46,24): error FS0001: The type ''a array' does not match the type 'Map<string,obj> list'
            
    --- FAIL: TestFSCompiler_GoldenOutput/nested_type (1.33s)
        golden.go:70: process error: ❌ fsi error: exit status 1
            
            
            /tmp/TestFSCompiler_GoldenOutput2649731433/071/main.fsx(6,9): error FS3524: Expecting expression
            
    --- FAIL: TestFSCompiler_GoldenOutput/pow_builtin (0.26s)
        golden.go:70: process error: ❌ type error: error[T003]: unknown function: pow
              --> /workspace/mochi/tests/compiler/fs/pow_builtin.mochi:1:7
            
              1 | print(pow(2,3))
                |       ^
            
            help:
              Ensure the function is defined before it's called.
    --- FAIL: TestFSCompiler_GoldenOutput/set_ops (2.10s)
        golden.go:70: process error: runtime mismatch
            
            --- F# ---
            [|1; 2; 3; 4|]
            [|1; 2|]
            [|3|]
            [|1; 2; 3|]
            
            --- VM ---
            1 2 3 4
            1 2
            3
            1 2 3
    --- FAIL: TestFSCompiler_GoldenOutput/simple_struct (1.86s)
        golden.go:70: process error: runtime mismatch
            
            --- F# ---
            "Alice"
            30
            
            --- VM ---
            Alice
            30
    --- FAIL: TestFSCompiler_GoldenOutput/str_builtin (1.68s)
        golden.go:70: process error: runtime mismatch
            
            --- F# ---
            "123"
            
            --- VM ---
            123
    --- FAIL: TestFSCompiler_GoldenOutput/string_concat (1.99s)
        golden.go:70: process error: runtime mismatch
            
            --- F# ---
            "hello world"
            
            --- VM ---
            hello world
    --- FAIL: TestFSCompiler_GoldenOutput/string_index#01 (1.68s)
        golden.go:70: process error: runtime mismatch
            
            --- F# ---
            "e"
            
            --- VM ---
            e
    --- FAIL: TestFSCompiler_GoldenOutput/string_negative_index (1.96s)
        golden.go:70: process error: runtime mismatch
            
            --- F# ---
            "o"
            
            --- VM ---
            o
    --- FAIL: TestFSCompiler_GoldenOutput/string_slice (1.67s)
        golden.go:70: process error: runtime mismatch
            
            --- F# ---
            "ell"
            
            --- VM ---
            ell
    --- FAIL: TestFSCompiler_GoldenOutput/string_slice_negative (1.74s)
        golden.go:70: process error: runtime mismatch
            
            --- F# ---
            "ell"
            
            --- VM ---
            ell
    --- FAIL: TestFSCompiler_GoldenOutput/tpch_q1 (1.65s)
        golden.go:70: process error: ❌ fsi error: exit status 1
            
            
            /tmp/TestFSCompiler_GoldenOutput2649731433/081/main.fsx(68,5): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (67:17). Try indenting this further.
            To continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7.
            
            
            
            /tmp/TestFSCompiler_GoldenOutput2649731433/081/main.fsx(68,5): error FS0058: Unexpected syntax or possible incorrect indentation: this token is offside of context started at position (67:17). Try indenting this further.
            To continue using non-conforming indentation, pass the '--strict-indentation-' flag to the compiler, or set the language version to F# 7.
            
    --- FAIL: TestFSCompiler_GoldenOutput/tpch_q2 (1.79s)
        golden.go:70: process error: ❌ fsi error: exit status 1
            
            
            /tmp/TestFSCompiler_GoldenOutput2649731433/082/main.fsx(57,55): error FS0001: This expression was expected to have type
                'int'    
            but here has type
                'string'    
            
    --- FAIL: TestFSCompiler_GoldenOutput/update_statement (1.77s)
        golden.go:70: process error: runtime mismatch
            
            --- F# ---
            "ok"
            update adult status ... PASS
            
            --- VM ---
            ok
FAIL
FAIL	mochi/compile/x/fs	154.557s
FAIL
