#include <algorithm>
#include <iostream>
#include <map>
#include <numeric>
#include <unordered_map>
#include <utility>
#include <vector>

struct __struct1 {
  decltype(1) n;
  decltype(std::string("a")) v;
};
int main() {
  auto items = std::vector<decltype(__struct1{1, std::string("a")})>{
      __struct1{1, std::string("a")}, __struct1{1, std::string("b")},
      __struct1{2, std::string("c")}};
  auto result = ([&]() {
    std::vector<std::pair<decltype(std::declval<__struct1>().n),
                          decltype(std::declval<__struct1>().v)>>
        __items;
    for (auto i : items) {
      __items.push_back({i.n, i.v});
    }
    std::sort(__items.begin(), __items.end(),
              [](auto &a, auto &b) { return a.first < b.first; });
    std::vector<decltype(std::declval<__struct1>().v)> __res;
    for (auto &p : __items)
      __res.push_back(p.second);
    return __res;
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
