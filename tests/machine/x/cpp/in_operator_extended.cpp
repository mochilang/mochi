#include <algorithm>
#include <iostream>
#include <vector>

struct __struct1 {
  decltype(1) a;
};
inline bool operator==(const __struct1 &a, const __struct1 &b) {
  return a.a == b.a;
}
inline bool operator!=(const __struct1 &a, const __struct1 &b) {
  return !(a == b);
}
std::vector<int> xs = std::vector<decltype(1)>{1, 2, 3};
auto ys = ([]() {
  std::vector<int> __items;
  for (auto x : xs) {
    if (!(((x % 2) == 1)))
      continue;
    __items.push_back(x);
  }
  return __items;
})();
auto m = __struct1{1};
auto s = std::string("hello");

int main() {
  {
    std::cout << std::boolalpha
              << (std::find(ys.begin(), ys.end(), 1) != ys.end());
    std::cout << std::endl;
  }
  {
    std::cout << std::boolalpha
              << (std::find(ys.begin(), ys.end(), 2) != ys.end());
    std::cout << std::endl;
  }
  {
    std::cout << std::boolalpha
              << (std::find(m.begin(), m.end(), std::string("a")) != m.end());
    std::cout << std::endl;
  }
  {
    std::cout << std::boolalpha
              << (std::find(m.begin(), m.end(), std::string("b")) != m.end());
    std::cout << std::endl;
  }
  {
    std::cout << std::boolalpha
              << (s.find(std::string("ell")) != std::string::npos);
    std::cout << std::endl;
  }
  {
    std::cout << std::boolalpha
              << (s.find(std::string("foo")) != std::string::npos);
    std::cout << std::endl;
  }
  return 0;
}
