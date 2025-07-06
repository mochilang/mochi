# C++ roundtrip VM test failures

## tests/vm/valid/append_builtin.mochi

```
cpp2mochi error: tests/vm/valid/append_builtin.cpp.out: line 7:17: clang++: exit status 1: <stdin>:7:17: error: use of undeclared identifier 'append'
    7 |   std::cout << (append(a, 3)) << std::endl;
      |                 ^
1 error generated.

6:      vector<int> a = vector<int>{1, 2};
7:>>>   std::cout << (append(a, 3)) << std::endl;
                    ^
8:      return 0;
```

