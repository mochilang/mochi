// Generated by Mochi compiler v0.10.28 on 2006-01-02T15:04:05Z
#include <iostream>

auto k = 2;

int inc(int x) { return (x + k); }

int main() {
  std::cout << inc(3) << std::endl;
  return 0;
}
