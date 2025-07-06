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
    compiler_test.go:158: updated: /workspace/mochi/tests/compiler/valid/closure.fs.out
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
    compiler_test.go:158: updated: /workspace/mochi/tests/compiler/valid/fold_pure_let.fs.out
=== RUN   TestFSCompiler_GoldenOutput/for_list_collection
    compiler_test.go:158: updated: /workspace/mochi/tests/compiler/valid/for_list_collection.fs.out
=== RUN   TestFSCompiler_GoldenOutput/for_loop
signal: interrupt
FAIL	mochi/compile/x/fs	29.765s

