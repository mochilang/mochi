--- FAIL: TestPascalCompiler_GoldenOutput (9.44s)
    --- FAIL: TestPascalCompiler_GoldenOutput/break_continue (0.13s)
        golden.go:70: process error: vm mismatch
            
            --- Pascal ---
            odd number:1
            odd number:2
            odd number:3
            odd number:4
            odd number:5
            odd number:6
            odd number:7
            odd number:8
            odd number:9
            
            --- VM ---
            odd number: 1
            odd number: 3
            odd number: 5
            odd number: 7
    --- FAIL: TestPascalCompiler_GoldenOutput/closure (0.08s)
        golden.go:70: process error: ❌ fpc error: exit status 1
            Free Pascal Compiler version 3.2.2+dfsg-32 [2024/01/05] for x86_64
            Copyright (c) 1993-2021 by Florian Klaempfl and others
            Target OS: Linux for x86-64
            Compiling /tmp/TestPascalCompiler_GoldenOutput3752949164/002/prog.pas
            prog.pas(9,33) Error: Type identifier expected
            prog.pas(9,33) Fatal: Syntax error, ";" expected but "FUNCTION" found
            Fatal: Compilation aborted
            Error: /usr/bin/ppcx64 returned an error exitcode
    --- FAIL: TestPascalCompiler_GoldenOutput/cross_join (0.26s)
        golden.go:70: process error: vm mismatch
            
            --- Pascal ---
            --- Cross Join: All order-customer pairs ---
            Order0(customerId:0, total: $0) paired with
            Order0(customerId:0, total: $0) paired with
            Order0(customerId:0, total: $0) paired with
            Order0(customerId:0, total: $0) paired with
            Order0(customerId:0, total: $0) paired with
            Order0(customerId:0, total: $0) paired with
            Order0(customerId:0, total: $0) paired with
            Order0(customerId:0, total: $0) paired with
            Order0(customerId:0, total: $0) paired with
            
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
    --- FAIL: TestPascalCompiler_GoldenOutput/fold_pure_let (0.33s)
        golden.go:70: process error: vm mismatch
            
            --- Pascal ---
            100
            10
            
            --- VM ---
            55
            10
    --- FAIL: TestPascalCompiler_GoldenOutput/fun_expr_in_let (0.13s)
        golden.go:70: process error: ❌ fpc error: exit status 1
            Free Pascal Compiler version 3.2.2+dfsg-32 [2024/01/05] for x86_64
            Copyright (c) 1993-2021 by Florian Klaempfl and others
            Target OS: Linux for x86-64
            Compiling /tmp/TestPascalCompiler_GoldenOutput3752949164/009/prog.pas
            prog.pas(10,28) Fatal: Syntax error, ":" expected but ")" found
            Fatal: Compilation aborted
            Error: /usr/bin/ppcx64 returned an error exitcode
    --- FAIL: TestPascalCompiler_GoldenOutput/generate_echo (0.00s)
        golden.go:70: process error: ❌ compile error: generative model expressions not supported
    --- FAIL: TestPascalCompiler_GoldenOutput/generate_embedding (0.00s)
        golden.go:70: process error: ❌ compile error: generative model expressions not supported
    --- FAIL: TestPascalCompiler_GoldenOutput/grouped_expr (0.19s)
        golden.go:70: process error: vm mismatch
            
            --- Pascal ---
            7
            
            --- VM ---
            9
    --- FAIL: TestPascalCompiler_GoldenOutput/join (0.08s)
        golden.go:70: process error: ❌ fpc error: exit status 1
            Free Pascal Compiler version 3.2.2+dfsg-32 [2024/01/05] for x86_64
            Copyright (c) 1993-2021 by Florian Klaempfl and others
            Target OS: Linux for x86-64
            Compiling /tmp/TestPascalCompiler_GoldenOutput3752949164/012/prog.pas
            prog.pas(63,20) Warning: Variable "o" does not seem to be initialized
            prog.pas(64,25) Error: Identifier not found "c"
            prog.pas(69,11) Error: Identifier not found "c"
            prog.pas(71,35) Error: Identifier not found "c"
            prog.pas(82) Fatal: There were 3 errors compiling module, stopping
            Fatal: Compilation aborted
            Error: /usr/bin/ppcx64 returned an error exitcode
    --- FAIL: TestPascalCompiler_GoldenOutput/join_filter_pag (0.09s)
        golden.go:70: process error: ❌ fpc error: exit status 1
            Free Pascal Compiler version 3.2.2+dfsg-32 [2024/01/05] for x86_64
            Copyright (c) 1993-2021 by Florian Klaempfl and others
            Target OS: Linux for x86-64
            Compiling /tmp/TestPascalCompiler_GoldenOutput3752949164/013/prog.pas
            prog.pas(99,32) Warning: Variable "p" does not seem to be initialized
            prog.pas(100,31) Error: Identifier not found "o"
            prog.pas(105,11) Error: Identifier not found "o"
            prog.pas(107,27) Error: Identifier not found "o"
            prog.pas(108,20) Error: Identifier not found "o"
            prog.pas(109,41) Error: Incompatible types: got "{Array Of Const/Constant Open} Array of TFPGMap$2$crcA49C6E68" expected "TArray$1$crcE7D92CDE"
            prog.pas(109,35) Error: Incompatible types: got "TFPGMap<System.Variant,System.Variant>" expected "TFPGMap<System.ShortString,System.Variant>"
            prog.pas(110,38) Error: Identifier not found "o"
            prog.pas(110,21) Error: Incompatible type for arg no. 3: Got "{Array Of Const/Constant Open} Array of TArray$1$crcE41905E8", expected "{Open} Array Of Pointer"
            prog.pas(119,17) Error: identifier idents no member "person"
            prog.pas(119,27) Error: identifier idents no member "spent"
            prog.pas(122) Fatal: There were 10 errors compiling module, stopping
            Fatal: Compilation aborted
            Error: /usr/bin/ppcx64 returned an error exitcode
    --- FAIL: TestPascalCompiler_GoldenOutput/local_recursion (0.00s)
        golden.go:70: process error: ❌ compile error: union types not supported
    --- FAIL: TestPascalCompiler_GoldenOutput/map_iterate (0.08s)
        golden.go:70: process error: ❌ fpc error: exit status 1
            Free Pascal Compiler version 3.2.2+dfsg-32 [2024/01/05] for x86_64
            Copyright (c) 1993-2021 by Florian Klaempfl and others
            Target OS: Linux for x86-64
            Compiling /tmp/TestPascalCompiler_GoldenOutput3752949164/019/prog.pas
            prog.pas(17,8) Error: Incompatible types: got "TFPGMap<System.ShortString,System.LongInt>" expected "TFPGMap<System.LongInt,System.Boolean>"
            prog.pas(23,18) Error: Operator is not overloaded: "LongInt" + "TFPGMap$2$crc50650EB1"
            prog.pas(21,12) Error: Cannot find an enumerator for the type "TFPGMap$2$crc50650EB1"
            prog.pas(27) Fatal: There were 3 errors compiling module, stopping
            Fatal: Compilation aborted
            Error: /usr/bin/ppcx64 returned an error exitcode
    --- FAIL: TestPascalCompiler_GoldenOutput/map_ops (0.08s)
        golden.go:70: process error: ❌ fpc error: exit status 1
            Free Pascal Compiler version 3.2.2+dfsg-32 [2024/01/05] for x86_64
            Copyright (c) 1993-2021 by Florian Klaempfl and others
            Target OS: Linux for x86-64
            Compiling /tmp/TestPascalCompiler_GoldenOutput3752949164/020/prog.pas
            prog.pas(15,8) Error: Incompatible types: got "TFPGMap<System.ShortString,System.LongInt>" expected "TFPGMap<System.LongInt,System.LongInt>"
            prog.pas(21) Fatal: There were 1 errors compiling module, stopping
            Fatal: Compilation aborted
            Error: /usr/bin/ppcx64 returned an error exitcode
    --- FAIL: TestPascalCompiler_GoldenOutput/match_expr (0.08s)
        golden.go:70: process error: ❌ fpc error: exit status 1
            Free Pascal Compiler version 3.2.2+dfsg-32 [2024/01/05] for x86_64
            Copyright (c) 1993-2021 by Florian Klaempfl and others
            Target OS: Linux for x86-64
            Compiling /tmp/TestPascalCompiler_GoldenOutput3752949164/022/prog.pas
            prog.pas(21,7) Fatal: Syntax error, ";" expected but "ELSE" found
            Fatal: Compilation aborted
            Error: /usr/bin/ppcx64 returned an error exitcode
    --- FAIL: TestPascalCompiler_GoldenOutput/reduce (0.08s)
        golden.go:70: process error: ❌ fpc error: exit status 1
            Free Pascal Compiler version 3.2.2+dfsg-32 [2024/01/05] for x86_64
            Copyright (c) 1993-2021 by Florian Klaempfl and others
            Target OS: Linux for x86-64
            Compiling /tmp/TestPascalCompiler_GoldenOutput3752949164/024/prog.pas
            prog.pas(11,10) Error: Illegal expression
            prog.pas(19,11) Error: Identifier not found "reduce"
            prog.pas(19,24) Error: Wrong number of parameters specified for call to "add"
            prog.pas(9,10) Error: Found declaration: add(LongInt;LongInt):LongInt;
            prog.pas(21) Fatal: There were 4 errors compiling module, stopping
            Fatal: Compilation aborted
            Error: /usr/bin/ppcx64 returned an error exitcode
    --- FAIL: TestPascalCompiler_GoldenOutput/stream_on_emit (0.00s)
        golden.go:70: process error: ❌ compile error: agents and streams not supported
    --- FAIL: TestPascalCompiler_GoldenOutput/two_sum (0.09s)
        golden.go:70: process error: ❌ fpc error: exit status 1
            Free Pascal Compiler version 3.2.2+dfsg-32 [2024/01/05] for x86_64
            Copyright (c) 1993-2021 by Florian Klaempfl and others
            Target OS: Linux for x86-64
            Compiling /tmp/TestPascalCompiler_GoldenOutput3752949164/027/prog.pas
            prog.pas(19,11) Error: Ordinal expression expected
            prog.pas(21,26) Error: Identifier not found "_indexList"
            prog.pas(21,50) Fatal: Syntax error, ")" expected but "," found
            Fatal: Compilation aborted
            Error: /usr/bin/ppcx64 returned an error exitcode
    --- FAIL: TestPascalCompiler_GoldenOutput/union_inorder (0.00s)
        golden.go:70: process error: ❌ compile error: union types not supported
    --- FAIL: TestPascalCompiler_GoldenOutput/union_match (0.00s)
        golden.go:70: process error: ❌ compile error: union types not supported
    --- FAIL: TestPascalCompiler_GoldenOutput/union_slice (0.00s)
        golden.go:70: process error: ❌ compile error: union types not supported
    --- FAIL: TestPascalCompiler_GoldenOutput/abs (0.12s)
        golden.go:70: process error: runtime output mismatch
            
            --- VM ---
            <nil>
            
            --- Want ---
            5
            
    --- FAIL: TestPascalCompiler_GoldenOutput/bool_ops (0.12s)
        golden.go:70: process error: runtime output mismatch
            
            --- VM ---
            false
            true
            true
            
            --- Want ---
            FALSE
            TRUE
            TRUE
            
    --- FAIL: TestPascalCompiler_GoldenOutput/break_continue#01 (0.16s)
        golden.go:70: process error: runtime output mismatch
            
            --- VM ---
            odd number: 1
            odd number: 3
            odd number: 5
            odd number: 7
            
            --- Want ---
            odd number:1
            odd number:3
            odd number:5
            odd number:7
            
    --- FAIL: TestPascalCompiler_GoldenOutput/dataset_sort (0.16s)
        golden.go:70: process error: vm mismatch
            
            --- Pascal ---
            --- Top products (excluding most expensive) ---
            Monitorcosts $300
            Headphonescosts $200
            Keyboardcosts $100
            
            --- VM ---
            --- Top products (excluding most expensive) ---
            Smartphone costs $ 900
            Tablet costs $ 600
            Monitor costs $ 300
    --- FAIL: TestPascalCompiler_GoldenOutput/enum (0.17s)
        golden.go:70: process error: runtime output mismatch
            
            --- VM ---
            map[__name:Green]
            
            --- Want ---
            Green
            
    --- FAIL: TestPascalCompiler_GoldenOutput/fetch_builtin (0.15s)
        golden.go:70: process error: ❌ run error: exit status 217
            An unhandled exception occurred at $000000000045E879:
            EFOpenError: Unable to open file "tests/compiler/pas/fetch_builtin.json": No such file or directory
              $000000000045E879
              $000000000045E613
              $0000000000401BA8
            
    --- FAIL: TestPascalCompiler_GoldenOutput/group_by (0.08s)
        golden.go:70: process error: ❌ fpc error: exit status 1
            Free Pascal Compiler version 3.2.2+dfsg-32 [2024/01/05] for x86_64
            Copyright (c) 1993-2021 by Florian Klaempfl and others
            Target OS: Linux for x86-64
            Compiling /tmp/TestPascalCompiler_GoldenOutput3752949164/040/prog.pas
            prog.pas(14,67) Error: Type identifier expected
            prog.pas(14,67) Fatal: Syntax error, ")" expected but "FUNCTION" found
            Fatal: Compilation aborted
            Error: /usr/bin/ppcx64 returned an error exitcode
    --- FAIL: TestPascalCompiler_GoldenOutput/if_else#01 (0.29s)
        golden.go:70: process error: runtime output mismatch
            
            --- VM ---
            -1
            0
            3
            
            --- Want ---
            -1
            0
            1
            
    --- FAIL: TestPascalCompiler_GoldenOutput/join#01 (0.10s)
        golden.go:70: process error: ❌ fpc error: exit status 1
            Free Pascal Compiler version 3.2.2+dfsg-32 [2024/01/05] for x86_64
            Copyright (c) 1993-2021 by Florian Klaempfl and others
            Target OS: Linux for x86-64
            Compiling /tmp/TestPascalCompiler_GoldenOutput3752949164/044/prog.pas
            prog.pas(63,20) Warning: Variable "o" does not seem to be initialized
            prog.pas(64,25) Error: Identifier not found "c"
            prog.pas(69,11) Error: Identifier not found "c"
            prog.pas(71,35) Error: Identifier not found "c"
            prog.pas(82) Fatal: There were 3 errors compiling module, stopping
            Fatal: Compilation aborted
            Error: /usr/bin/ppcx64 returned an error exitcode
    --- FAIL: TestPascalCompiler_GoldenOutput/list_concat (0.13s)
        golden.go:70: process error: runtime output mismatch
            
            --- VM ---
            <nil>
            <nil>
            <nil>
            <nil>
            
            --- Want ---
            1
            2
            3
            4
            
    --- FAIL: TestPascalCompiler_GoldenOutput/list_slice (0.08s)
        golden.go:70: process error: ❌ fpc error: exit status 1
            Free Pascal Compiler version 3.2.2+dfsg-32 [2024/01/05] for x86_64
            Copyright (c) 1993-2021 by Florian Klaempfl and others
            Target OS: Linux for x86-64
            Compiling /tmp/TestPascalCompiler_GoldenOutput3752949164/047/prog.pas
            prog.pas(25,89) Error: Can't read or write variables of this type
            prog.pas(27) Fatal: There were 1 errors compiling module, stopping
            Fatal: Compilation aborted
            Error: /usr/bin/ppcx64 returned an error exitcode
    --- FAIL: TestPascalCompiler_GoldenOutput/load_save_json (0.08s)
        golden.go:70: process error: ❌ fpc error: exit status 1
            Free Pascal Compiler version 3.2.2+dfsg-32 [2024/01/05] for x86_64
            Copyright (c) 1993-2021 by Florian Klaempfl and others
            Target OS: Linux for x86-64
            Compiling /tmp/TestPascalCompiler_GoldenOutput3752949164/048/prog.pas
            prog.pas(21,7) Error: Identifier not found "TJSONDeStreamer"
            prog.pas(21,22) Error: Error in type definition
            prog.pas(31,21) Warning: function result variable of a managed type does not seem to be initialized
            prog.pas(32,11) Error: Identifier not found "TJSONDeStreamer"
            prog.pas(35,12) Error: Illegal qualifier
            prog.pas(37,10) Error: Illegal qualifier
            prog.pas(48,7) Error: Identifier not found "TJSONStreamer"
            prog.pas(48,20) Error: Error in type definition
            prog.pas(51,9) Error: Identifier not found "TJSONStreamer"
            prog.pas(56,19) Error: Illegal qualifier
            prog.pas(62,8) Error: Illegal qualifier
            prog.pas(74,42) Fatal: illegal character "'"'" ($22)
            Fatal: Compilation aborted
            Error: /usr/bin/ppcx64 returned an error exitcode
    --- FAIL: TestPascalCompiler_GoldenOutput/match_enum (0.08s)
        golden.go:70: process error: ❌ fpc error: exit status 1
            Free Pascal Compiler version 3.2.2+dfsg-32 [2024/01/05] for x86_64
            Copyright (c) 1993-2021 by Florian Klaempfl and others
            Target OS: Linux for x86-64
            Compiling /tmp/TestPascalCompiler_GoldenOutput3752949164/049/prog.pas
            prog.pas(22,7) Fatal: Syntax error, ";" expected but "ELSE" found
            Fatal: Compilation aborted
            Error: /usr/bin/ppcx64 returned an error exitcode
    --- FAIL: TestPascalCompiler_GoldenOutput/package_import (0.13s)
        golden.go:70: process error: ❌ vm run error: call stack exceeded 1024 frames
    --- FAIL: TestPascalCompiler_GoldenOutput/string_split_join (0.13s)
        golden.go:70: process error: runtime output mismatch
            
            --- VM ---
            <nil>
            
            --- Want ---
            a-b-c
            
    --- FAIL: TestPascalCompiler_GoldenOutput/tpch_q1 (0.10s)
        golden.go:70: process error: ❌ fpc error: exit status 1
            Free Pascal Compiler version 3.2.2+dfsg-32 [2024/01/05] for x86_64
            Copyright (c) 1993-2021 by Florian Klaempfl and others
            Target OS: Linux for x86-64
            Compiling /tmp/TestPascalCompiler_GoldenOutput3752949164/063/prog.pas
            prog.pas(17,24) Error: Identifier not found "_sumList"
            prog.pas(17,41) Error: Operator is not overloaded: "TArray$1$crc90DE1C48" / "Int64"
            prog.pas(20,65) Error: Type identifier expected
            prog.pas(20,65) Fatal: Syntax error, ")" expected but "FUNCTION" found
            Fatal: Compilation aborted
            Error: /usr/bin/ppcx64 returned an error exitcode
    --- FAIL: TestPascalCompiler_GoldenOutput/two_sum#01 (0.09s)
        golden.go:70: process error: ❌ fpc error: exit status 1
            Free Pascal Compiler version 3.2.2+dfsg-32 [2024/01/05] for x86_64
            Copyright (c) 1993-2021 by Florian Klaempfl and others
            Target OS: Linux for x86-64
            Compiling /tmp/TestPascalCompiler_GoldenOutput3752949164/064/prog.pas
            prog.pas(19,11) Error: Ordinal expression expected
            prog.pas(21,26) Error: Identifier not found "_indexList"
            prog.pas(21,50) Fatal: Syntax error, ")" expected but "," found
            Fatal: Compilation aborted
            Error: /usr/bin/ppcx64 returned an error exitcode
    --- FAIL: TestPascalCompiler_GoldenOutput/update_statement (0.16s)
        golden.go:70: process error: ❌ run error: exit status 217
            An unhandled exception occurred at $00000000004012D4:
            Exception: expect failed
              $00000000004012D4
              $000000000040168E
            
FAIL
FAIL	mochi/compile/x/pas	9.450s
FAIL
