# Pascal roundtrip compiler tests

## tests/compiler/valid/break_continue.mochi

```
output mismatch
-- got --
odd number:1
odd number:2
odd number:3
odd number:4
odd number:5
odd number:6
odd number:7
odd number:8
odd number:9
-- want --
odd number: 1
odd number: 3
odd number: 5
odd number: 7
```

## tests/compiler/valid/closure.mochi

```
fpc error: exit status 1
Free Pascal Compiler version 3.2.2+dfsg-32 [2024/01/05] for x86_64
Copyright (c) 1993-2021 by Florian Klaempfl and others
Target OS: Linux for x86-64
Compiling closure.pas.out
closure.pas.out(9,33) Error: Type identifier expected
closure.pas.out(9,33) Fatal: Syntax error, ";" expected but "FUNCTION" found
Fatal: Compilation aborted
Error: /usr/bin/ppcx64 returned an error exitcode

```

## tests/compiler/valid/cross_join.mochi

```
output mismatch
-- got --
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
-- want --
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
```

## tests/compiler/valid/fold_pure_let.mochi

```
output mismatch
-- got --
100
10
-- want --
55
10
```

## tests/compiler/valid/fun_expr_in_let.mochi

```
fpc error: exit status 1
Free Pascal Compiler version 3.2.2+dfsg-32 [2024/01/05] for x86_64
Copyright (c) 1993-2021 by Florian Klaempfl and others
Target OS: Linux for x86-64
Compiling fun_expr_in_let.pas.out
fun_expr_in_let.pas.out(10,28) Fatal: Syntax error, ":" expected but ")" found
Fatal: Compilation aborted
Error: /usr/bin/ppcx64 returned an error exitcode

```

## tests/compiler/valid/generate_echo.mochi

```
compile error: generative model expressions not supported
```

## tests/compiler/valid/generate_embedding.mochi

```
compile error: generative model expressions not supported
```

## tests/compiler/valid/grouped_expr.mochi

```
output mismatch
-- got --
7
-- want --
9
```

## tests/compiler/valid/join.mochi

```
fpc error: exit status 1
Free Pascal Compiler version 3.2.2+dfsg-32 [2024/01/05] for x86_64
Copyright (c) 1993-2021 by Florian Klaempfl and others
Target OS: Linux for x86-64
Compiling join.pas.out
join.pas.out(63,20) Warning: Variable "o" does not seem to be initialized
join.pas.out(64,25) Error: Identifier not found "c"
join.pas.out(69,11) Error: Identifier not found "c"
join.pas.out(71,35) Error: Identifier not found "c"
join.pas.out(82) Fatal: There were 3 errors compiling module, stopping
Fatal: Compilation aborted
Error: /usr/bin/ppcx64 returned an error exitcode

```

## tests/compiler/valid/join_filter_pag.mochi

```
fpc error: exit status 1
Free Pascal Compiler version 3.2.2+dfsg-32 [2024/01/05] for x86_64
Copyright (c) 1993-2021 by Florian Klaempfl and others
Target OS: Linux for x86-64
Compiling join_filter_pag.pas.out
join_filter_pag.pas.out(99,32) Warning: Variable "p" does not seem to be initialized
join_filter_pag.pas.out(100,31) Error: Identifier not found "o"
join_filter_pag.pas.out(105,11) Error: Identifier not found "o"
join_filter_pag.pas.out(107,27) Error: Identifier not found "o"
join_filter_pag.pas.out(108,20) Error: Identifier not found "o"
join_filter_pag.pas.out(109,41) Error: Incompatible types: got "{Array Of Const/Constant Open} Array of TFPGMap$2$crcA49C6E68" expected "TArray$1$crcE7D92CDE"
join_filter_pag.pas.out(109,35) Error: Incompatible types: got "TFPGMap<System.Variant,System.Variant>" expected "TFPGMap<System.ShortString,System.Variant>"
join_filter_pag.pas.out(110,38) Error: Identifier not found "o"
join_filter_pag.pas.out(110,21) Error: Incompatible type for arg no. 3: Got "{Array Of Const/Constant Open} Array of TArray$1$crcE41905E8", expected "{Open} Array Of Pointer"
join_filter_pag.pas.out(119,17) Error: identifier idents no member "person"
join_filter_pag.pas.out(119,27) Error: identifier idents no member "spent"
join_filter_pag.pas.out(122) Fatal: There were 10 errors compiling module, stopping
Fatal: Compilation aborted
Error: /usr/bin/ppcx64 returned an error exitcode

```

## tests/compiler/valid/local_recursion.mochi

```
compile error: union types not supported
```

## tests/compiler/valid/map_iterate.mochi

```
fpc error: exit status 1
Free Pascal Compiler version 3.2.2+dfsg-32 [2024/01/05] for x86_64
Copyright (c) 1993-2021 by Florian Klaempfl and others
Target OS: Linux for x86-64
Compiling map_iterate.pas.out
map_iterate.pas.out(17,8) Error: Incompatible types: got "TFPGMap<System.ShortString,System.LongInt>" expected "TFPGMap<System.LongInt,System.Boolean>"
map_iterate.pas.out(23,18) Error: Operator is not overloaded: "LongInt" + "TFPGMap$2$crc50650EB1"
map_iterate.pas.out(21,12) Error: Cannot find an enumerator for the type "TFPGMap$2$crc50650EB1"
map_iterate.pas.out(27) Fatal: There were 3 errors compiling module, stopping
Fatal: Compilation aborted
Error: /usr/bin/ppcx64 returned an error exitcode

```

## tests/compiler/valid/map_ops.mochi

```
fpc error: exit status 1
Free Pascal Compiler version 3.2.2+dfsg-32 [2024/01/05] for x86_64
Copyright (c) 1993-2021 by Florian Klaempfl and others
Target OS: Linux for x86-64
Compiling map_ops.pas.out
map_ops.pas.out(15,8) Error: Incompatible types: got "TFPGMap<System.ShortString,System.LongInt>" expected "TFPGMap<System.LongInt,System.LongInt>"
map_ops.pas.out(21) Fatal: There were 1 errors compiling module, stopping
Fatal: Compilation aborted
Error: /usr/bin/ppcx64 returned an error exitcode

```

## tests/compiler/valid/match_expr.mochi

```
fpc error: exit status 1
Free Pascal Compiler version 3.2.2+dfsg-32 [2024/01/05] for x86_64
Copyright (c) 1993-2021 by Florian Klaempfl and others
Target OS: Linux for x86-64
Compiling match_expr.pas.out
match_expr.pas.out(21,7) Fatal: Syntax error, ";" expected but "ELSE" found
Fatal: Compilation aborted
Error: /usr/bin/ppcx64 returned an error exitcode

```

## tests/compiler/valid/reduce.mochi

```
fpc error: exit status 1
Free Pascal Compiler version 3.2.2+dfsg-32 [2024/01/05] for x86_64
Copyright (c) 1993-2021 by Florian Klaempfl and others
Target OS: Linux for x86-64
Compiling reduce.pas.out
reduce.pas.out(11,10) Error: Illegal expression
reduce.pas.out(19,11) Error: Identifier not found "reduce"
reduce.pas.out(19,24) Error: Wrong number of parameters specified for call to "add"
reduce.pas.out(9,10) Error: Found declaration: add(LongInt;LongInt):LongInt;
reduce.pas.out(21) Fatal: There were 4 errors compiling module, stopping
Fatal: Compilation aborted
Error: /usr/bin/ppcx64 returned an error exitcode

```

## tests/compiler/valid/stream_on_emit.mochi

```
compile error: agents and streams not supported
```

## tests/compiler/valid/two_sum.mochi

```
fpc error: exit status 1
Free Pascal Compiler version 3.2.2+dfsg-32 [2024/01/05] for x86_64
Copyright (c) 1993-2021 by Florian Klaempfl and others
Target OS: Linux for x86-64
Compiling two_sum.pas.out
two_sum.pas.out(19,11) Error: Ordinal expression expected
two_sum.pas.out(21,26) Error: Identifier not found "_indexList"
two_sum.pas.out(21,50) Fatal: Syntax error, ")" expected but "," found
Fatal: Compilation aborted
Error: /usr/bin/ppcx64 returned an error exitcode

```

## tests/compiler/valid/union_inorder.mochi

```
compile error: union types not supported
```

## tests/compiler/valid/union_match.mochi

```
compile error: union types not supported
```

## tests/compiler/valid/union_slice.mochi

```
compile error: union types not supported
```

