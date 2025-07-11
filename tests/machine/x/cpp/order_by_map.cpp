#include <algorithm>
#include <iostream>
#include <utility>
#include <vector>

struct Data {
  decltype(1) a;
  decltype(2) b;
};
inline bool operator==(const Data &a, const Data &b) {
  return a.a == b.a && a.b == b.b;
}
inline bool operator!=(const Data &a, const Data &b) { return !(a == b); }
int main() {
  std::vector<Data> data =
      std::vector<Data>{Data{1, 2}, Data{1, 1}, Data{0, 5}};
  auto sorted = ([&]() {
    std::vector<std::pair<Data, Data>> __items;
    for (auto x : data) {
      __items.push_back({Data{x.a, x.b}, x});
    }
    std::sort(__items.begin(), __items.end(), [](auto &a, auto &b) {
      return std::tie(a.first.a, a.first.b) < std::tie(b.first.a, b.first.b);
    });
    std::vector<Data> __res;
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
