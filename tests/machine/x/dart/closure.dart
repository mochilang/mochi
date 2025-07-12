Function makeAdder(int n) {
  return (x) => (x as num) + n;
}

var add10 = makeAdder(10);

void main() {
  print(add10(7));
}
