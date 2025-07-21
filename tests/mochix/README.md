# Mochix Transpiler Outputs

This folder contains the generated source files for `hello.mochi` across many transpiler targets. Each `hello.*` file is produced by running:

```
go run -tags slow ./cmd/mochix build[|x] --target <lang> tests/mochix/hello.mochi
```

The table below tracks which transpilers successfully generate output.

- [x] C (`hello.c`)
- [x] Clojure (`hello.clj.out`)
- [x] COBOL (`hello.cob.out`)
- [x] C++ (`hello.cpp.out`)
- [x] C# (`hello.cs`)
- [x] Dart (`hello.dart`)
- [x] Erlang (`hello.erl`)
- [x] Elixir (`hello.ex.out`)
- [x] Fortran (`hello.f90.out`)
- [x] F# (`hello.fs`)
- [x] Go (`hello.go.out`)
- [x] Haskell (`hello.hs`)
- [x] Java (`hello.java.out`)
- [x] Kotlin (`hello.kt.out`)
- [x] Lua (`hello.lua.out`)
- [x] OCaml (`hello.ml.out`)
- [x] Pascal (`hello.pas.out`)
- [x] PHP (`hello.php`)
- [x] Prolog (`hello.pl`)
- [x] Python (`hello.py.out`)
- [x] Racket (`hello.rkt.out`)
- [x] Ruby (`hello.rb.out`)
- [x] Rust (`hello.rs.out`)
- [x] Scala (`hello.scala.out`)
- [x] Scheme (`hello.scm.out`)
- [ ] Smalltalk (`hello.smalltalk.out`)
- [x] ST (`hello.st.out`)
- [x] Swift (`hello.swift.out`)
- [x] TypeScript (`hello.ts`)
- [x] Zig (`hello.zig.out`)
