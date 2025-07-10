#include <iostream>

struct __struct1 {
  decltype(0) n;
};
struct Counter {
  int n;
};

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
