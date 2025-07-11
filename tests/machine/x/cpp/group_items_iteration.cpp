#include <algorithm>
#include <iostream>
#include <map>
#include <string>
#include <utility>
#include <vector>

struct __struct3 {
  decltype(g.key) tag;
  decltype(total) total;
};
inline bool operator==(const __struct3 &a, const __struct3 &b) {
  return a.tag == b.tag && a.total == b.total;
}
inline bool operator!=(const __struct3 &a, const __struct3 &b) {
  return !(a == b);
}
template <typename T, typename U>
std::vector<T> __append(const std::vector<T> &v, const U &x) {
  auto r = v;
  r.push_back(x);
  return r;
}
a.key == b.key &&a.items == b.items;
}
inline bool operator!=(const __struct2 &a, const __struct2 &b) {
  return !(a == b);
}
template <typename T, typename U>
std::vector<T> __append(const std::vector<T> &v, const U &x) {
  auto r = v;
  r.push_back(x);
  return r;
}
int main() {
  std::vector<__struct1> data =
      std::vector<decltype(__struct1{std::string("a"), 1})>{
          __struct1{std::string("a"), 1}, __struct1{std::string("a"), 2},
          __struct1{std::string("b"), 3}};
  auto groups = ([&]() {
    std::map<decltype(d.tag), std::vector<__struct1>> __groups;
    for (auto d : data) {
      __groups[d.tag].push_back(__struct1{d});
    }
    std::vector<__struct2> __items;
    for (auto &kv : __groups) {
      __items.push_back(__struct2{kv.first, kv.second});
    }
    return __items;
  })();
  std::vector<__struct3> tmp = std::vector<__struct3>{};
  for (auto g : groups) {
    auto total = 0;
    for (auto x : g.items) {
      total = (total + x.val);
    }
    tmp = __append(tmp, __struct3{g.key, total});
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
