int outer(int x) {
  int inner(int y) {
    return (x + y as int);
  }

  return inner(5);
}

void main() {
  print(outer(3));
}
