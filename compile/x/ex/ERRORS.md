# Errors

- break_continue: ❌ elixir run error: exit status 1
odd number: 1
** (throw) :continue
    /tmp/TestExCompiler_ValidVMbreak_continue2881377174/001/main.exs:8: anonymous fn/2 in Main.main/0
    (elixir 1.14.0) lib/enum.ex:2468: Enum."-reduce/3-lists^foldl/2-0-"/3
    /tmp/TestExCompiler_ValidVMbreak_continue2881377174/001/main.exs:6: Main.main/0

- fold_pure_let: output mismatch
- generate_echo: output mismatch
- generate_embedding: output mismatch
- list_set: ❌ elixir run error: exit status 1
warning: map update will fail with a 'badmap' exception
  /tmp/TestExCompiler_ValidVMlist_set4279041909/001/main.exs:6

** (BadMapError) expected a map, got: [1, 2]
    /tmp/TestExCompiler_ValidVMlist_set4279041909/001/main.exs:6: Main.main/0
    (elixir 1.14.0) lib/code.ex:1245: Code.require_file/2

- local_recursion: ❌ elixir run error: exit status 1
warning: variable "helper" does not exist and is being expanded to "helper()", please use parentheses to remove the ambiguity or change the variable name
  /tmp/TestExCompiler_ValidVMlocal_recursion2287344290/001/main.exs:14: Main.fromList/1

warning: variable "helper" does not exist and is being expanded to "helper()", please use parentheses to remove the ambiguity or change the variable name
  /tmp/TestExCompiler_ValidVMlocal_recursion2287344290/001/main.exs:14: Main.fromList/1

warning: variable "l" does not exist and is being expanded to "l()", please use parentheses to remove the ambiguity or change the variable name
  /tmp/TestExCompiler_ValidVMlocal_recursion2287344290/001/main.exs:33: Main.inorder/1

warning: variable "v" does not exist and is being expanded to "v()", please use parentheses to remove the ambiguity or change the variable name
  /tmp/TestExCompiler_ValidVMlocal_recursion2287344290/001/main.exs:33: Main.inorder/1

warning: variable "r" does not exist and is being expanded to "r()", please use parentheses to remove the ambiguity or change the variable name
  /tmp/TestExCompiler_ValidVMlocal_recursion2287344290/001/main.exs:33: Main.inorder/1

** (CompileError) /tmp/TestExCompiler_ValidVMlocal_recursion2287344290/001/main.exs:33: invalid function call :Elixir.Node.()
    (stdlib 4.3.1.3) lists.erl:1462: :lists.mapfoldl_1/3

- reduce: ❌ elixir run error: exit status 1
warning: variable "reduce" does not exist and is being expanded to "reduce()", please use parentheses to remove the ambiguity or change the variable name
  /tmp/TestExCompiler_ValidVMreduce1332127364/001/main.exs:13: Main.main/0

warning: variable "add" does not exist and is being expanded to "add()", please use parentheses to remove the ambiguity or change the variable name
  /tmp/TestExCompiler_ValidVMreduce1332127364/001/main.exs:13: Main.main/0

warning: undefined function reduce/0 (expected Main to define such a function or for it to be imported, but none are available)
  /tmp/TestExCompiler_ValidVMreduce1332127364/001/main.exs:13

** (CompileError) /tmp/TestExCompiler_ValidVMreduce1332127364/001/main.exs:13: undefined function add/0 (expected Main to define such a function or for it to be imported, but none are available)


- union_inorder: ❌ elixir run error: exit status 1
warning: variable "l" does not exist and is being expanded to "l()", please use parentheses to remove the ambiguity or change the variable name
  /tmp/TestExCompiler_ValidVMunion_inorder2306613507/001/main.exs:12: Main.inorder/1

warning: variable "v" does not exist and is being expanded to "v()", please use parentheses to remove the ambiguity or change the variable name
  /tmp/TestExCompiler_ValidVMunion_inorder2306613507/001/main.exs:12: Main.inorder/1

warning: variable "r" does not exist and is being expanded to "r()", please use parentheses to remove the ambiguity or change the variable name
  /tmp/TestExCompiler_ValidVMunion_inorder2306613507/001/main.exs:12: Main.inorder/1

** (CompileError) /tmp/TestExCompiler_ValidVMunion_inorder2306613507/001/main.exs:12: invalid function call :Elixir.Node.()
    (stdlib 4.3.1.3) lists.erl:1462: :lists.mapfoldl_1/3

- union_match: output mismatch
