Function makeAdder(int n) {
  return (x) => (x as num) + n;
}

void main() {
  var add10 = makeAdder(10);
  print(add10(7));
}
