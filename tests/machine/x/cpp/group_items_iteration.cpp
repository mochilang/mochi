#include <algorithm>
#include <iostream>
#include <utility>
#include <vector>

struct __struct1 {
  decltype(std::string("a")) tag;
  decltype(1) val;
};
struct __struct2 {
  decltype(std::declval<__struct1>().tag) key;
  std::vector<__struct1> items;
};
struct __struct3 {
  decltype(std::declval<__struct2>().key) tag;
  decltype(total) total;
};
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
  auto tmp = std::vector<int>{};
  for (auto g : groups) {
    auto total = 0;
    for (auto x : g.items) {
      total = (total + x.val);
    }
    tmp = ([&](auto v) {
      v.push_back(__struct3{g.key, total});
      return v;
    })(tmp);
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
    for (size_t i = 0; i < __tmp1.size(); ++i) {
      if (i)
        std::cout << ' ';
      std::cout << std::boolalpha << __tmp1[i];
    }
    std::cout << std::endl;
  }
  return 0;
}
