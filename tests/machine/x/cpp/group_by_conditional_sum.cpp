#include <algorithm>
#include <iostream>
#include <numeric>
#include <utility>
#include <vector>

struct __struct1 {
  decltype(std::string("a")) cat;
  decltype(10) val;
  bool flag;
};
struct __struct2 {
  decltype(std::declval<__struct1>().cat) key;
  std::vector<__struct1> items;
};
struct __struct3 {
  decltype(std::declval<__struct2>().key) cat;
  bool share;
};
std::vector<__struct1> items =
    std::vector<decltype(__struct1{std::string("a"), 10, true})>{
        __struct1{std::string("a"), 10, true},
        __struct1{std::string("a"), 5, false},
        __struct1{std::string("b"), 20, true}};
auto result = ([]() {
  std::vector<__struct2> __groups;
  for (auto i : items) {
    auto __key = i.cat;
    bool __found = false;
    for (auto &__g : __groups) {
      if (__g.key == __key) {
        __g.items.push_back(__struct1{i});
        __found = true;
        break;
      }
    }
    if (!__found) {
      __groups.push_back(
          __struct2{__key, std::vector<__struct1>{__struct1{i}}});
    }
  }
  std::vector<std::pair<decltype(std::declval<__struct2>().key), __struct3>>
      __items;
  for (auto &g : __groups) {
    __items.push_back(
        {g.key,
         __struct3{
             g.key,
             (([&](auto v) { return std::accumulate(v.begin(), v.end(), 0); })(
                  ([&]() {
                    std::vector<decltype((std::declval<__struct1>().flag
                                              ? std::declval<__struct1>().val
                                              : 0))>
                        __items;
                    for (auto x : g.items) {
                      __items.push_back((x.flag ? x.val : 0));
                    }
                    return __items;
                  })()) /
              ([&](auto v) {
                return std::accumulate(v.begin(), v.end(), 0);
              })(([]() {
                std::vector<decltype(std::declval<__struct1>().val)> __items;
                for (auto x : g.items) {
                  __items.push_back(x.val);
                }
                return __items;
              })()))}});
  }
  std::sort(__items.begin(), __items.end(),
            [](auto &a, auto &b) { return a.first < b.first; });
  std::vector<__struct3> __res;
  for (auto &p : __items)
    __res.push_back(p.second);
  return __res;
})();

int main() {
  {
    auto __tmp1 = result;
    for (size_t i = 0; i < __tmp1.size(); ++i) {
      if (i)
        std::cout << ' ';
      std::cout << "<struct>";
    }
    std::cout << std::endl;
  }
  return 0;
}
