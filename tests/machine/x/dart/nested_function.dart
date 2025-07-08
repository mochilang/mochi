int outer(int x) {
  return inner(5);
}

void main() {
  print(outer(3));
}
