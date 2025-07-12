#include <algorithm>
#include <iostream>
#include <map>
#include <string>
#include <utility>
#include <vector>

struct Data {
  decltype(std::string("a")) tag;
  decltype(1) val;
};
inline bool operator==(const Data &a, const Data &b) {
  return a.tag == b.tag && a.val == b.val;
}
inline bool operator!=(const Data &a, const Data &b) { return !(a == b); }
struct __struct2 {
  decltype(std::declval<Data>().tag) key;
  std::vector<Data> items;
};
inline bool operator==(const __struct2 &a, const __struct2 &b) {
  return a.key == b.key && a.items == b.items;
}
inline bool operator!=(const __struct2 &a, const __struct2 &b) {
  return !(a == b);
}
struct __struct3 {
  decltype(std::declval<Data>().key) tag;
  int total;
};
inline bool operator==(const __struct3 &a, const __struct3 &b) {
  return a.tag == b.tag && a.total == b.total;
}
inline bool operator!=(const __struct3 &a, const __struct3 &b) {
  return !(a == b);
}
int main() {
  std::vector<Data> data =
      std::vector<Data>{Data{std::string("a"), 1}, Data{std::string("a"), 2},
                        Data{std::string("b"), 3}};
  auto groups = ([&]() {
    std::map<decltype(std::declval<Data>().tag), std::vector<Data>> __groups;
    for (auto d : data) {
      __groups[d.tag].push_back(Data{d});
    }
    std::vector<__struct2> __items;
    for (auto &kv : __groups) {
      __items.push_back(__struct2{kv.first, kv.second});
    }
    return __items;
  })();
  std::vector<int> tmp = std::vector<int>{};
  for (auto g : groups) {
    auto total = 0;
    for (auto x : g.items) {
      total = (total + x.val);
    }
    tmp.push_back(__struct3{g.key, total});
  }
  auto result = ([&]() {
    std::vector<std::pair<decltype(std::declval<__struct3>().tag), __struct3>>
        __items;
    for (auto r : tmp) {
      __items.push_back({r.tag, r});
    }
    std::sort(__items.begin(), __items.end(),
              [](auto &a, auto &b) { return a.first < b.first; });
    std::vector<__struct3> __res;
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
      std::cout << "<struct>";
    }
    std::cout << std::endl;
  }
  return 0;
}
