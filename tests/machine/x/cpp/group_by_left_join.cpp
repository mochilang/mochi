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
  decltype(std::string("Alice")) name;
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
  decltype(100) id;
  decltype(1) customerId;
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
  std::cout << "\"customerId\":";
  __json(v.customerId);
  std::cout << "}";
}
struct __struct3 {
  __struct1 c;
  __struct2 o;
};
inline void __json(const __struct3 &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"c\":";
  __json(v.c);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"o\":";
  __json(v.o);
  std::cout << "}";
}
struct __struct4 {
  decltype(std::declval<__struct1>().name) key;
  std::vector<__struct3> items;
};
struct __struct5 {
  decltype(std::declval<__struct4>().key) name;
  bool count;
};
inline void __json(const __struct5 &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"name\":";
  __json(v.name);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"count\":";
  __json(v.count);
  std::cout << "}";
}
int main() {
  std::vector<__struct1> customers =
      std::vector<decltype(__struct1{1, std::string("Alice")})>{
          __struct1{1, std::string("Alice")}, __struct1{2, std::string("Bob")},
          __struct1{3, std::string("Charlie")}};
  std::vector<__struct2> orders = std::vector<decltype(__struct2{100, 1})>{
      __struct2{100, 1}, __struct2{101, 1}, __struct2{102, 2}};
  auto stats = ([&]() {
    std::vector<__struct4> __groups;
    for (auto c : customers) {
      {
        bool __matched0 = false;
        for (auto o : orders) {
          if (!((o.customerId == c.id)))
            continue;
          __matched0 = true;
          auto __key = c.name;
          bool __found = false;
          for (auto &__g : __groups) {
            if (__g.key == __key) {
              __g.items.push_back(__struct3{c, o});
              __found = true;
              break;
            }
          }
          if (!__found) {
            __groups.push_back(
                __struct4{__key, std::vector<__struct3>{__struct3{c, o}}});
          }
        }
        if (!__matched0) {
          auto o = std::decay_t<decltype(*(orders).begin())>{};
          auto __key = c.name;
          bool __found = false;
          for (auto &__g : __groups) {
            if (__g.key == __key) {
              __g.items.push_back(__struct3{c, o});
              __found = true;
              break;
            }
          }
          if (!__found) {
            __groups.push_back(
                __struct4{__key, std::vector<__struct3>{__struct3{c, o}}});
          }
        }
      }
    }
    std::vector<__struct5> __items;
    for (auto &g : __groups) {
      __items.push_back(__struct5{g.key, ((int)([&]() {
                                            std::vector<__struct3> __items;
                                            for (auto r : g.items) {
                                              if (!(r.o))
                                                continue;
                                              __items.push_back(r);
                                            }
                                            return __items;
                                          })()
                                              .size())});
    }
    return __items;
  })();
  {
    std::cout << std::boolalpha << std::string("--- Group Left Join ---");
    std::cout << std::endl;
  }
  for (auto s : stats) {
    {
      std::cout << std::boolalpha << s.name;
      std::cout << ' ';
      std::cout << std::boolalpha << std::string("orders:");
      std::cout << ' ';
      std::cout << std::boolalpha << s.count;
      std::cout << std::endl;
    }
  }
  return 0;
}
