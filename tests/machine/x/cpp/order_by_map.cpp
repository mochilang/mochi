#include <algorithm>
#include <iostream>
#include <map>
#include <numeric>
#include <unordered_map>
#include <utility>
#include <vector>

struct __struct1 {
  decltype(1) a;
  decltype(2) b;
};
int main() {
  auto data = std::vector<decltype(__struct1{1, 2})>{
      __struct1{1, 2}, __struct1{1, 1}, __struct1{0, 5}};
  auto sorted = ([&]() {
    std::vector<std::pair<decltype(std::declval<__struct1>().a), __struct1>>
        __items;
    for (auto x : data) {
      __items.push_back({__struct1{x.a, x.b}, x});
    }
    std::sort(__items.begin(), __items.end(),
              [](auto &a, auto &b) { return a.first < b.first; });
    std::vector<__struct1> __res;
    for (auto &p : __items)
      __res.push_back(p.second);
    return __res;
  })();
  {
    std::cout << std::boolalpha << sorted;
    std::cout << std::endl;
  }
  return 0;
}
