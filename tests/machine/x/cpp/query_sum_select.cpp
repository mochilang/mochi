#include <algorithm>
#include <iostream>
#include <map>
#include <numeric>
#include <unordered_map>
#include <utility>
#include <vector>

int main() {
  std::vector<int> nums = std::vector<decltype(1)>{1, 2, 3};
  auto result = ([&]() {
    std::vector<decltype((
        [&](auto v) { return std::accumulate(v.begin(), v.end(), 0); })(n))>
        __items;
    for (auto n : nums) {
      if (!((n > 1)))
        continue;
      __items.push_back(
          ([&](auto v) { return std::accumulate(v.begin(), v.end(), 0); })(n));
    }
    return __items;
  })();
  {
    auto __tmp1 = result;
    for (size_t i = 0; i < __tmp1.size(); ++i) {
      if (i)
        std::cout << ' ';
      std::cout << std::boolalpha << __tmp1[i];
    }
    std::cout << std::endl;
  }
  return 0;
}
