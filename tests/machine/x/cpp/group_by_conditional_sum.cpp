#include <algorithm>
#include <iostream>
#include <map>
#include <numeric>
#include <unordered_map>
#include <utility>
#include <vector>

template <typename T> void __json(const T &);
inline void __json(int v) { std::cout << v; }
inline void __json(double v) { std::cout << v; }
inline void __json(bool v) { std::cout << (v ? "true" : "false"); }
inline void __json(const std::string &v) { std::cout << "\"" << v << "\""; }
inline void __json(const char *v) { std::cout << "\"" << v << "\""; }
template <typename T> void __json(const std::vector<T> &v) {
  std::cout << "[";
  bool first = true;
  for (const auto &x : v) {
    if (!first)
      std::cout << ",";
    first = false;
    __json(x);
  }
  std::cout << "]";
}
template <typename K, typename V> void __json(const std::map<K, V> &m) {
  std::cout << "{";
  bool first = true;
  for (const auto &kv : m) {
    if (!first)
      std::cout << ",";
    first = false;
    __json(kv.first);
    std::cout << ":";
    __json(kv.second);
  }
  std::cout << "}";
}
template <typename K, typename V>
void __json(const std::unordered_map<K, V> &m) {
  std::cout << "{";
  bool first = true;
  for (const auto &kv : m) {
    if (!first)
      std::cout << ",";
    first = false;
    __json(kv.first);
    std::cout << ":";
    __json(kv.second);
  }
  std::cout << "}";
}

struct __struct1 {
  decltype(std::string("a")) cat;
  decltype(10) val;
  bool flag;
};
inline void __json(const __struct1 &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"cat\":";
  __json(v.cat);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"val\":";
  __json(v.val);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"flag\":";
  __json(v.flag);
  std::cout << "}";
}
struct __struct2 {
  decltype(std::declval<__struct1>().cat) key;
  std::vector<__struct1> items;
};
struct __struct3 {
  decltype(std::declval<__struct2>().key) cat;
  bool share;
};
inline void __json(const __struct3 &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"cat\":";
  __json(v.cat);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"share\":";
  __json(v.share);
  std::cout << "}";
}
int main() {
  std::vector<__struct1> items =
      std::vector<decltype(__struct1{std::string("a"), 10, true})>{
          __struct1{std::string("a"), 10, true},
          __struct1{std::string("a"), 5, false},
          __struct1{std::string("b"), 20, true}};
  auto result = ([&]() {
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
               (([&](auto v) {
                  return std::accumulate(v.begin(), v.end(), 0);
                })(([&]() {
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
                })(([&]() {
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
