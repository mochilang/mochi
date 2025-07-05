func.func @main() {
  %c = arith.constant "hello" : !llvm.ptr<i8>
  return
}
