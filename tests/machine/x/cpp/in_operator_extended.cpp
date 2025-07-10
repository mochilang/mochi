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
  decltype(1) a;
};
inline void __json(const __struct1 &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"a\":";
  __json(v.a);
  std::cout << "}";
}
int main() {
  std::vector<int> xs = std::vector<decltype(1)>{1, 2, 3};
  auto ys = ([&]() {
    std::vector<int> __items;
    for (auto x : xs) {
      if (!(((x % 2) == 1)))
        continue;
      __items.push_back(x);
    }
    return __items;
  })();
  {
    std::cout << std::boolalpha
              << (std::find(ys.begin(), ys.end(), 1) != ys.end());
    std::cout << std::endl;
  }
  {
    std::cout << std::boolalpha
              << (std::find(ys.begin(), ys.end(), 2) != ys.end());
    std::cout << std::endl;
  }
  auto m = __struct1{1};
  {
    std::cout << std::boolalpha
              << (std::find(m.begin(), m.end(), std::string("a")) != m.end());
    std::cout << std::endl;
  }
  {
    std::cout << std::boolalpha
              << (std::find(m.begin(), m.end(), std::string("b")) != m.end());
    std::cout << std::endl;
  }
  auto s = std::string("hello");
  {
    std::cout << std::boolalpha
              << (s.find(std::string("ell")) != std::string::npos);
    std::cout << std::endl;
  }
  {
    std::cout << std::boolalpha
              << (s.find(std::string("foo")) != std::string::npos);
    std::cout << std::endl;
  }
  return 0;
}
