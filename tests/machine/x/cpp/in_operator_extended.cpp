#include <algorithm>
#include <iostream>
#include <map>
#include <numeric>
#include <unordered_map>
#include <utility>
#include <vector>

struct __struct1 {
  decltype(1) a;
};
int main() {
  std::vector<int> xs = std::vector<decltype(1)>{1, 2, 3};
  auto ys = ([&]() {
    std::vector<int> __items;
    for (auto x : xs) {
      if (!(((x % 2) == 1)))
        continue;
      __items.push_back(x);
    }
    return __items;
  })();
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
  auto m = __struct1{1};
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
  auto s = std::string("hello");
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
