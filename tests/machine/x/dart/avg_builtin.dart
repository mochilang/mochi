void main() {
  print(([1, 2, 3].isEmpty ? 0 : [1, 2, 3].reduce((a, b) => a + b) / [1, 2, 3].length));
}
