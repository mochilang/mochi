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
std::vector<__struct1> data = std::vector<decltype(__struct1{1, 2})>{
    __struct1{1, 2}, __struct1{1, 1}, __struct1{0, 5}};
auto sorted = ([]() {
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

int main() {
  {
    auto __tmp1 = sorted;
    for (size_t i = 0; i < __tmp1.size(); ++i) {
      if (i)
        std::cout << ' ';
      std::cout << "<struct>";
    }
    std::cout << std::endl;
  }
  return 0;
}
