int add(int a, int b) {
  return (a + b as int);
}

var add5 = (b) => add(5, b);

void main() {
  print(add5(3));
}
