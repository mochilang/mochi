int fib(int n) {
  if ((n <= 1)) {
    return n;
  }
  return (fib((n - 1)) + fib((n - 2)));
}

void main() {
  print(fib(0));
  print(fib(1));
  print(fib(6));
}
