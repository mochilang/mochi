// Generated by Mochi compiler v0.10.26 on 2025-07-16T09:21:08Z
#include <iostream>

float sqrtApprox(float x) {
  auto guess = x;
  auto i = 0;
  while ((i < 20)) {
    guess = (((guess + (x / guess))) / 2);
    i = (i + 1);
  }
  return guess;
}

int main() {
  auto n = 10;
  auto sum = 0;
  auto x = 1;
  while ((x <= n)) {
    sum = (sum + ((x) * (x)));
    x = (x + 1);
  }
  auto rms = sqrtApprox((sum / (n)));
  std::cout << std::to_string(rms) << std::endl;
  return 0;
}
