// Solution for SPOJ TEST - Life, the Universe, and Everything
// https://www.spoj.com/problems/TEST
import 'dart:io';

void main() {
  while (true) {
    final line = stdin.readLineSync();
    if (line == null) break;
    final n = int.parse(line.trim());
    if (n == 42) break;
    print(n);
  }
}
