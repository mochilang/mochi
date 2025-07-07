bool boom(int a, int b) {
  print('boom');
  return true;
}

void main() {
  print(false && boom(1, 2));
  print(true || boom(1, 2));
}
