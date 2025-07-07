int add(int a, int b) => a + b;

void main() {
  var add5 = (int x) => add(5, x);
  print(add5(3));
}
