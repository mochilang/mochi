#include <algorithm>
#include <iostream>
#include <map>
#include <numeric>
#include <unordered_map>
#include <utility>
#include <vector>

int main() {
  auto nums = std::vector<decltype(1)>{1, 2, 3};
  auto result = ([&]() {
    std::vector<decltype(std::accumulate(n.begin(), n.end(), 0))> __items;
    for (auto n : nums) {
      if (!((n > 1)))
        continue;
      __items.push_back(std::accumulate(n.begin(), n.end(), 0));
    }
    return __items;
  })();
  {
    std::cout << std::boolalpha << result;
    std::cout << std::endl;
  }
  return 0;
}
