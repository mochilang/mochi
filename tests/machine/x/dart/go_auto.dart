
class testpkg {
  static int Add(int a, int b) => a + b;
  static const double Pi = 3.14;
  static const int Answer = 42;
}

void main() {
  print(testpkg.Add(2, 3));
  print(testpkg.Pi);
  print(testpkg.Answer);
}
