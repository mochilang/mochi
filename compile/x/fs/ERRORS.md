=== RUN   TestFSCompiler_GoldenOutput
=== RUN   TestFSCompiler_GoldenOutput/break_continue
    compiler_test.go:146: skipping unsupported file: runtime mismatch
        
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
=== RUN   TestFSCompiler_GoldenOutput/closure
=== RUN   TestFSCompiler_GoldenOutput/cross_join
    compiler_test.go:146: skipping unsupported file: runtime mismatch
        
        --- F# ---
        "--- Cross Join: All order-customer pairs ---"
        ("Order", 100, "(customerId:", 1, ", total: $", 250, ") paired with", "Alice")
        ("Order", 100, "(customerId:", 1, ", total: $", 250, ") paired with", "Bob")
        ("Order", 100, "(customerId:", 1, ", total: $", 250, ") paired with", "Charlie")
        ("Order", 101, "(customerId:", 2, ", total: $", 125, ") paired with", "Alice")
        ("Order", 101, "(customerId:", 2, ", total: $", 125, ") paired with", "Bob")
        ("Order", 101, "(customerId:", 2, ", total: $", 125, ") paired with", "Charlie")
        ("Order", 102, "(customerId:", 1, ", total: $", 300, ") paired with", "Alice")
        ("Order", 102, "(customerId:", 1, ", total: $", 300, ") paired with", "Bob")
        ("Order", 102, "(customerId:", 1, ", total: $", 300, ") paired with", "Charlie")
        
        --- VM ---
        --- Cross Join: All order-customer pairs ---
        Order 100 (customerId: 1 , total: $ 250 ) paired with Alice
        Order 100 (customerId: 1 , total: $ 250 ) paired with Bob
        Order 100 (customerId: 1 , total: $ 250 ) paired with Charlie
        Order 101 (customerId: 2 , total: $ 125 ) paired with Alice
        Order 101 (customerId: 2 , total: $ 125 ) paired with Bob
        Order 101 (customerId: 2 , total: $ 125 ) paired with Charlie
        Order 102 (customerId: 1 , total: $ 300 ) paired with Alice
        Order 102 (customerId: 1 , total: $ 300 ) paired with Bob
        Order 102 (customerId: 1 , total: $ 300 ) paired with Charlie
=== RUN   TestFSCompiler_GoldenOutput/fold_pure_let
=== RUN   TestFSCompiler_GoldenOutput/for_list_collection
=== RUN   TestFSCompiler_GoldenOutput/for_loop
=== RUN   TestFSCompiler_GoldenOutput/for_string_collection
    compiler_test.go:146: skipping unsupported file: runtime mismatch
        
        --- F# ---
        "h"
        "i"
        
        --- VM ---
        h
        i
=== RUN   TestFSCompiler_GoldenOutput/fun_call
=== RUN   TestFSCompiler_GoldenOutput/fun_expr_in_let
=== RUN   TestFSCompiler_GoldenOutput/generate_echo
    compiler_test.go:146: skipping unsupported file: runtime mismatch
        
        --- F# ---
        /tmp/TestFSCompiler_GoldenOutput2308848553/010/main.fsx(3,48): warning FS0046: The identifier 'params' is reserved for future use by F#
        
        "echo hello"
        
        --- VM ---
        <nil>
=== RUN   TestFSCompiler_GoldenOutput/generate_embedding
    compiler_test.go:146: skipping unsupported file: runtime mismatch
        
        --- F# ---
        /tmp/TestFSCompiler_GoldenOutput2308848553/011/main.fsx(3,47): warning FS0046: The identifier 'params' is reserved for future use by F#
        
        2
        
        --- VM ---
        0
=== RUN   TestFSCompiler_GoldenOutput/grouped_expr
=== RUN   TestFSCompiler_GoldenOutput/if_else
    compiler_test.go:146: skipping unsupported file: runtime mismatch
        
        --- F# ---
        "big"
        
        --- VM ---
        big
=== RUN   TestFSCompiler_GoldenOutput/join
    compiler_test.go:146: skipping unsupported file: runtime mismatch
        
        --- F# ---
        "--- Orders with customer info ---"
        ("Order", 100, "by", "Alice", "- $", 250)
        ("Order", 101, "by", "Bob", "- $", 125)
        ("Order", 102, "by", "Alice", "- $", 300)
        
        --- VM ---
        --- Orders with customer info ---
        Order 100 by Alice - $ 250
        Order 101 by Bob - $ 125
        Order 102 by Alice - $ 300
=== RUN   TestFSCompiler_GoldenOutput/join_filter_pag
    compiler_test.go:146: skipping unsupported file: ❌ fsi error: exit status 1
        
        
        /tmp/TestFSCompiler_GoldenOutput2308848553/015/main.fsx(27,78): error FS0001: This expression was expected to have type
            'string'    
        but here has type
            'int'    
        
=== RUN   TestFSCompiler_GoldenOutput/len_builtin
=== RUN   TestFSCompiler_GoldenOutput/let_and_print
=== RUN   TestFSCompiler_GoldenOutput/list_index
=== RUN   TestFSCompiler_GoldenOutput/list_set
=== RUN   TestFSCompiler_GoldenOutput/local_recursion
signal: interrupt
FAIL	mochi/compile/x/fs	57.723s
FAIL
