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
  decltype(1) id;
  decltype(std::string("A")) name;
};
inline void __json(const __struct1 &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"id\":";
  __json(v.id);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"name\":";
  __json(v.name);
  std::cout << "}";
}
struct __struct2 {
  decltype(1) id;
  decltype(1) nation;
};
inline void __json(const __struct2 &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"id\":";
  __json(v.id);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"nation\":";
  __json(v.nation);
  std::cout << "}";
}
struct __struct3 {
  decltype(100) part;
  decltype(1) supplier;
  decltype(10) cost;
  decltype(2) qty;
};
inline void __json(const __struct3 &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"part\":";
  __json(v.part);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"supplier\":";
  __json(v.supplier);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"cost\":";
  __json(v.cost);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"qty\":";
  __json(v.qty);
  std::cout << "}";
}
struct __struct4 {
  decltype(std::declval<__struct3>().part) part;
  decltype((ps.cost * ps.qty)) value;
};
inline void __json(const __struct4 &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"part\":";
  __json(v.part);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"value\":";
  __json(v.value);
  std::cout << "}";
}
struct __struct5 {
  decltype(std::declval<__struct4>().part) key;
  std::vector<__struct4> items;
};
struct __struct6 {
  decltype(std::declval<__struct5>().key) part;
  bool total;
};
inline void __json(const __struct6 &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"part\":";
  __json(v.part);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"total\":";
  __json(v.total);
  std::cout << "}";
}
int main() {
  std::vector<__struct1> nations =
      std::vector<decltype(__struct1{1, std::string("A")})>{
          __struct1{1, std::string("A")}, __struct1{2, std::string("B")}};
  std::vector<__struct2> suppliers =
      std::vector<decltype(__struct2{1, 1})>{__struct2{1, 1}, __struct2{2, 2}};
  std::vector<__struct3> partsupp =
      std::vector<decltype(__struct3{100, 1, 10, 2})>{__struct3{100, 1, 10, 2},
                                                      __struct3{100, 2, 20, 1},
                                                      __struct3{200, 1, 5, 3}};
  auto filtered = ([&]() {
    std::vector<__struct4> __items;
    for (auto ps : partsupp) {
      for (auto s : suppliers) {
        if (!((s.id == ps.supplier)))
          continue;
        for (auto n : nations) {
          if (!((n.id == s.nation)))
            continue;
          if (!((n.name == std::string("A"))))
            continue;
          __items.push_back(__struct4{ps.part, (ps.cost * ps.qty)});
        }
      }
    }
    return __items;
  })();
  auto grouped = ([&]() {
    std::vector<__struct5> __groups;
    for (auto x : filtered) {
      auto __key = x.part;
      bool __found = false;
      for (auto &__g : __groups) {
        if (__g.key == __key) {
          __g.items.push_back(__struct4{x});
          __found = true;
          break;
        }
      }
      if (!__found) {
        __groups.push_back(
            __struct5{__key, std::vector<__struct4>{__struct4{x}}});
      }
    }
    std::vector<__struct6> __items;
    for (auto &g : __groups) {
      __items.push_back(__struct6{
          g.key, ([&](auto v) {
            return std::accumulate(v.begin(), v.end(), 0);
          })(([&]() {
            std::vector<decltype(std::declval<__struct4>().value)> __items;
            for (auto r : g.items) {
              __items.push_back(r.value);
            }
            return __items;
          })())});
    }
    return __items;
  })();
  {
    auto __tmp1 = grouped;
    for (size_t i = 0; i < __tmp1.size(); ++i) {
      if (i)
        std::cout << ' ';
      std::cout << "<struct>";
    }
    std::cout << std::endl;
  }
  return 0;
}
