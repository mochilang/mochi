#include <algorithm>
#include <iostream>
#include <numeric>
#include <string>
#include <utility>
#include <vector>

struct __struct1 {
  decltype(std::string("a")) cat;
  decltype(3) val;
};
inline bool operator==(const __struct1 &a, const __struct1 &b) {
  return a.cat == b.cat && a.val == b.val;
}
inline bool operator!=(const __struct1 &a, const __struct1 &b) {
  return !(a == b);
}
struct __struct2 {
  decltype(i) i;
};
inline bool operator==(const __struct2 &a, const __struct2 &b) {
  return a.i == b.i;
}
inline bool operator!=(const __struct2 &a, const __struct2 &b) {
  return !(a == b);
}
struct __struct3 {
  decltype(i.cat) key;
  std::vector<__struct2> items;
};
inline bool operator==(const __struct3 &a, const __struct3 &b) {
  return a.key == b.key && a.items == b.items;
}
inline bool operator!=(const __struct3 &a, const __struct3 &b) {
  return !(a == b);
}
struct __struct4 {
  decltype(std::declval<__struct3>().key) cat;
  bool total;
};
inline bool operator==(const __struct4 &a, const __struct4 &b) {
  return a.cat == b.cat && a.total == b.total;
}
inline bool operator!=(const __struct4 &a, const __struct4 &b) {
  return !(a == b);
}
int main() {
  auto items = std::vector<__struct1>{
      __struct1{std::string("a"), 3}, __struct1{std::string("a"), 1},
      __struct1{std::string("b"), 5}, __struct1{std::string("b"), 2}};
  auto grouped = ([&]() {
    std::vector<__struct3> __groups;
    for (auto i : items) {
      auto __key = i.cat;
      bool __found = false;
      for (auto &__g : __groups) {
        if (__g.key == __key) {
          __g.items.push_back(__struct2{i});
          __found = true;
          break;
        }
      }
      if (!__found) {
        __groups.push_back(
            __struct3{__key, std::vector<__struct2>{__struct2{i}}});
      }
    }
    std::vector<std::pair<double, __struct4>> __items;
    for (auto &g : __groups) {
      __items.push_back(
          {(-([&](auto v) { return std::accumulate(v.begin(), v.end(), 0); })(
               ([&]() {
                 std::vector<decltype(std::declval<__struct2>().val)> __items;
                 for (auto x : g.items) {
                   __items.push_back(x.val);
                 }
                 return __items;
               })())),
           __struct4{
               g.key, ([&](auto v) {
                 return std::accumulate(v.begin(), v.end(), 0);
               })(([&]() {
                 std::vector<decltype(std::declval<__struct2>().val)> __items;
                 for (auto x : g.items) {
                   __items.push_back(x.val);
                 }
                 return __items;
               })())}});
    }
    std::sort(__items.begin(), __items.end(),
              [](auto &a, auto &b) { return a.first < b.first; });
    std::vector<__struct4> __res;
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
