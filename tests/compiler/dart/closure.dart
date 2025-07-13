dynamic makeAdder(int n) {
  return (x) => (x + n);
}

dynamic add10 = makeAdder(10);

void main() {
  print(add10(7));
}
