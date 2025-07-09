#include <algorithm>
#include <iostream>
#include <map>
#include <numeric>
#include <unordered_map>
#include <utility>
#include <vector>

int main() {
  auto data = std::vector<decltype(1)>{1, 2};
  auto flag = exists(([&]() {
    std::vector<decltype(x)> __items;
    for (auto x : data) {
      if (!((x == 1)))
        continue;
      __items.push_back(x);
    }
    return __items;
  })());
  {
    std::cout << std::boolalpha << flag;
    std::cout << std::endl;
  }
  return 0;
}
