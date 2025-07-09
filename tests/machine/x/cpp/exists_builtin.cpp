#include <algorithm>
#include <iostream>
#include <map>
#include <numeric>
#include <unordered_map>
#include <utility>
#include <vector>

int main() {
  std::vector<int> data = std::vector<decltype(1)>{1, 2};
  auto flag = (!([&]() {
                  std::vector<int> __items;
                  for (auto x : data) {
                    if (!((x == 1)))
                      continue;
                    __items.push_back(x);
                  }
                  return __items;
                })()
                    .empty());
  {
    std::cout << std::boolalpha << flag;
    std::cout << std::endl;
  }
  return 0;
}
