#include <algorithm>
#include <iostream>
#include <string>
#include <utility>
#include <vector>

struct Item {
  decltype(1) n;
  decltype(std::string("a")) v;
};
inline bool operator==(const Item &a, const Item &b) {
  return a.n == b.n && a.v == b.v;
}
inline bool operator!=(const Item &a, const Item &b) { return !(a == b); }
int main() {
  std::vector<Item> items =
      std::vector<Item>{Item{1, std::string("a")}, Item{1, std::string("b")},
                        Item{2, std::string("c")}};
  auto result = ([&]() {
    std::vector<std::pair<decltype(std::declval<Item>().n),
                          decltype(std::declval<Item>().v)>>
        __items;
    for (auto i : items) {
      __items.push_back({i.n, i.v});
    }
    std::sort(__items.begin(), __items.end(),
              [](auto &a, auto &b) { return a.first < b.first; });
    std::vector<decltype(std::declval<Item>().v)> __res;
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
