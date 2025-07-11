#include <algorithm>
#include <iostream>
#include <string>
#include <unordered_map>
#include <vector>

int main() {
  auto xs = std::vector<int>{1, 2, 3};
  auto ys = ([&]() {
    std::vector<decltype(x)> __items;
    for (auto x : xs) {
      if (!(((x % 2) == 1)))
        continue;
      __items.push_back(x);
    }
    return __items;
  })();
  std::cout << std::boolalpha
            << (std::find(ys.begin(), ys.end(), 1) != ys.end()) << std::endl;
  std::cout << std::boolalpha
            << (std::find(ys.begin(), ys.end(), 2) != ys.end()) << std::endl;
  auto m = std::unordered_map<std::string, decltype(1)>{{std::string("a"), 1}};
  std::cout << std::boolalpha << (m.count(std::string("a")) > 0) << std::endl;
  std::cout << std::boolalpha << (m.count(std::string("b")) > 0) << std::endl;
  auto s = std::string("hello");
  std::cout << std::boolalpha
            << (s.find(std::string("ell")) != std::string::npos) << std::endl;
  std::cout << std::boolalpha
            << (s.find(std::string("foo")) != std::string::npos) << std::endl;
  return 0;
}
