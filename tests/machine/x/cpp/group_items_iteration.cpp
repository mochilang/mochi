#include <algorithm>
#include <iostream>
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
st __struct2 &b){ return a.key==b.key && a.items==b.items; }
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
    std::vector<__struct2> __groups;
    for (auto d : data) {
      auto __key = d.tag;
      bool __found = false;
      for (auto &__g : __groups) {
        if (__g.key == __key) {
          __g.items.push_back(__struct1{d});
          __found = true;
          break;
        }
      }
      if (!__found) {
        __groups.push_back(
            __struct2{__key, std::vector<__struct1>{__struct1{d}}});
      }
    }
    std::vector<__struct2> __items;
    for (auto &g : __groups) {
      __items.push_back(g);
    }
    return __items;
  })();
  std::vector<__tmp_type> tmp = std::vector<__tmp_type>{};
  for (auto g : groups) {
    auto total = 0;
    for (auto x : g.items) {
      total = (total + x.val);
    }
    tmp = __append(tmp, __struct3{g.key, total});
  }
  auto result = ([&]() {
    std::vector<std::pair<decltype(r.tag), decltype(r)>> __items;
    for (auto r : tmp) {
      __items.push_back({r.tag, r});
    }
    std::sort(__items.begin(), __items.end(),
              [](auto &a, auto &b) { return a.first < b.first; });
    std::vector<decltype(r)> __res;
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
