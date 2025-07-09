#include <algorithm>
#include <iostream>
#include <map>
#include <numeric>
#include <unordered_map>
#include <utility>
#include <vector>

struct __struct1 {
  decltype(0) n;
};
auto inc(auto c) { c.n = (c.n + 1); }

int main() {
  struct Counter {
    int n;
  };
  auto c = __struct1{0};
  inc(c);
  {
    std::cout << std::boolalpha << c.n;
    std::cout << std::endl;
  }
  return 0;
}
