#include <iostream>

struct Counter {
  int n;
};
inline bool operator==(const Counter &a, const Counter &b) {
  return a.n == b.n;
}
inline bool operator!=(const Counter &a, const Counter &b) { return !(a == b); }
struct __struct1 {
  decltype(0) n;
};
inline bool operator==(const __struct1 &a, const __struct1 &b) {
  return a.n == b.n;
}
inline bool operator!=(const __struct1 &a, const __struct1 &b) {
  return !(a == b);
}

auto inc(auto c) { c.n = (c.n + 1); }

int main() {
  auto c = __struct1{0};
  inc(c);
  {
    std::cout << std::boolalpha << c.n;
    std::cout << std::endl;
  }
  return 0;
}
