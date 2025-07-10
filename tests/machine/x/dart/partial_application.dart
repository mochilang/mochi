int add(int a, int b) {
  return a + b;
}

var add5 = (b) => add(5, b);

void main() {
  print(add5(3));
}
