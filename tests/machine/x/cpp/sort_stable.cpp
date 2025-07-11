#include <algorithm>
#include <iostream>
#include <string>
#include <utility>
#include <vector>

struct __struct1 {
  decltype(1) n;
  decltype(std::string("a")) v;
};
inline bool operator==(const __struct1 &a, const __struct1 &b) {
  return a.n == b.n && a.v == b.v;
}
inline bool operator!=(const __struct1 &a, const __struct1 &b) {
  return !(a == b);
}
int main() {
  auto items = std::vector<__struct1>{__struct1{1, std::string("a")},
                                      __struct1{1, std::string("b")},
                                      __struct1{2, std::string("c")}};
  auto result = ([&]() {
    std::vector<std::pair<decltype(i.n), decltype(i.v)>> __items;
    for (auto i : items) {
      __items.push_back({i.n, i.v});
    }
    std::sort(__items.begin(), __items.end(),
              [](auto &a, auto &b) { return a.first < b.first; });
    std::vector<decltype(i.v)> __res;
    for (auto &p : __items)
      __res.push_back(p.second);
    return __res;
  })();
  {
    auto __tmp1 = result;
    bool first = true;
    for (const auto &_x : __tmp1) {
      if (!first)
        std::cout << ' ';
      first = false;
      std::cout << std::boolalpha << _x;
    }
    std::cout << std::endl;
  }
  return 0;
}
