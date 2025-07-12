#include <algorithm>
#include <iostream>
#include <numeric>
#include <string>
#include <utility>
#include <vector>

struct Item {
  decltype(std::string("a")) cat;
  decltype(3) val;
};
inline bool operator==(const Item &a, const Item &b) {
  return a.cat == b.cat && a.val == b.val;
}
inline bool operator!=(const Item &a, const Item &b) { return !(a == b); }
struct __struct2 {
  decltype(std::declval<Item>().cat) key;
  std::vector<Item> items;
};
inline bool operator==(const __struct2 &a, const __struct2 &b) {
  return a.key == b.key && a.items == b.items;
}
inline bool operator!=(const __struct2 &a, const __struct2 &b) {
  return !(a == b);
}
struct Grouped {
  decltype(std::declval<__struct2>().key) cat;
  double total;
};
inline bool operator==(const Grouped &a, const Grouped &b) {
  return a.cat == b.cat && a.total == b.total;
}
inline bool operator!=(const Grouped &a, const Grouped &b) { return !(a == b); }
int main() {
  std::vector<Item> items =
      std::vector<Item>{Item{std::string("a"), 3}, Item{std::string("a"), 1},
                        Item{std::string("b"), 5}, Item{std::string("b"), 2}};
  auto grouped = ([&]() {
    std::vector<__struct2> __groups;
    for (auto i : items) {
      auto __key = i.cat;
      bool __found = false;
      for (auto &__g : __groups) {
        if (__g.key == __key) {
          __g.items.push_back(Item{i});
          __found = true;
          break;
        }
      }
      if (!__found) {
        __groups.push_back(__struct2{__key, std::vector<Item>{Item{i}}});
      }
    }
    std::vector<std::pair<double, Grouped>> __items;
    for (auto &g : __groups) {
      __items.push_back(
          {(-([&](auto v) { return std::accumulate(v.begin(), v.end(), 0.0); })(
               ([&]() {
                 std::vector<decltype(std::declval<Item>().val)> __items;
                 for (auto x : g.items) {
                   __items.push_back(x.val);
                 }
                 return __items;
               })())),
           Grouped{g.key, ([&](auto v) {
                     return std::accumulate(v.begin(), v.end(), 0.0);
                   })(([&]() {
                     std::vector<decltype(std::declval<Item>().val)> __items;
                     for (auto x : g.items) {
                       __items.push_back(x.val);
                     }
                     return __items;
                   })())}});
    }
    std::sort(__items.begin(), __items.end(),
              [](auto &a, auto &b) { return a.first < b.first; });
    std::vector<Grouped> __res;
    for (auto &p : __items)
      __res.push_back(p.second);
    return __res;
  })();
  {
    auto __tmp1 = grouped;
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
