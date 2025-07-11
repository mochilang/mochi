#include <algorithm>
#include <iostream>
#include <utility>
#include <vector>

struct __struct1 {
  decltype(1) a;
  decltype(2) b;
};
inline bool operator==(const __struct1 &a, const __struct1 &b) {
  return a.a == b.a && a.b == b.b;
}
inline bool operator!=(const __struct1 &a, const __struct1 &b) {
  return !(a == b);
}
int main() {
  std::vector<__struct1> data = std::vector<decltype(__struct1{1, 2})>{
      __struct1{1, 2}, __struct1{1, 1}, __struct1{0, 5}};
  auto sorted = ([&]() {
    std::vector<std::pair<__struct1, __struct1>> __items;
    for (auto x : data) {
      __items.push_back({__struct1{x.a, x.b}, x});
    }
    std::sort(__items.begin(), __items.end(), [](auto &a, auto &b) {
      return std::tie(a.first.a, a.first.b) < std::tie(b.first.a, b.first.b);
    });
    std::vector<__struct1> __res;
    for (auto &p : __items)
      __res.push_back(p.second);
    return __res;
  })();
  {
    auto __tmp1 = sorted;
    bool first = true;
    for (const auto &_x : __tmp1) {
      if (!first)
        std::cout << ' ';
      first = false;
      std::cout << "<struct>";
    }
    std::cout << std::endl;
  }
  return 0;
}
