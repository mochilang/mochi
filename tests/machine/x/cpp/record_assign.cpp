#include <iostream>

struct Counter {
  int n;
};
inline bool operator==(const Counter &a, const Counter &b) {
  return a.n == b.n;
}
inline bool operator!=(const Counter &a, const Counter &b) { return !(a == b); }

auto inc(Counter c) { c.n = (c.n + 1); }

int main() {
  auto c = Counter{0};
  inc(c);
  {
    std::cout << std::boolalpha << c.n;
    std::cout << std::endl;
  }
  return 0;
}
