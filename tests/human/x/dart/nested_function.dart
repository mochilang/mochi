int outer(int x) {
  int inner(int y) {
    return x + y;
  }
  return inner(5);
}

void main() {
  print(outer(3));
}
