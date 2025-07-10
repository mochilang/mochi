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
  decltype(100) orderId;
  decltype(std::string("a")) sku;
};
inline void __json(const __struct3 &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"orderId\":";
  __json(v.orderId);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"sku\":";
  __json(v.sku);
  std::cout << "}";
}
struct __struct4 {
  decltype(std::declval<__struct2>().id) orderId;
  decltype(std::declval<__struct1>().name) name;
  decltype(i) item;
};
inline void __json(const __struct4 &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"orderId\":";
  __json(v.orderId);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"name\":";
  __json(v.name);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"item\":";
  __json(v.item);
  std::cout << "}";
}
int main() {
  std::vector<__struct1> customers =
      std::vector<decltype(__struct1{1, std::string("Alice")})>{
          __struct1{1, std::string("Alice")}, __struct1{2, std::string("Bob")}};
  std::vector<__struct2> orders = std::vector<decltype(__struct2{100, 1})>{
      __struct2{100, 1}, __struct2{101, 2}};
  std::vector<__struct3> items =
      std::vector<decltype(__struct3{100, std::string("a")})>{
          __struct3{100, std::string("a")}};
  auto result = ([&]() {
    std::vector<__struct4> __items;
    for (auto o : orders) {
      for (auto c : customers) {
        if (!((o.customerId == c.id)))
          continue;
        {
          bool __matched1 = false;
          for (auto i : items) {
            if (!((o.id == i.orderId)))
              continue;
            __matched1 = true;
            __items.push_back(__struct4{o.id, c.name, i});
          }
          if (!__matched1) {
            auto i = std::decay_t<decltype(*(items).begin())>{};
            __items.push_back(__struct4{o.id, c.name, i});
          }
        }
      }
    }
    return __items;
  })();
  {
    std::cout << std::boolalpha << std::string("--- Left Join Multi ---");
    std::cout << std::endl;
  }
  for (auto r : result) {
    {
      std::cout << std::boolalpha << r.orderId;
      std::cout << ' ';
      std::cout << std::boolalpha << r.name;
      std::cout << ' ';
      std::cout << std::boolalpha << r.item;
      std::cout << std::endl;
    }
  }
  return 0;
}
