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
  int n;
  std::string l;
  bool b;
};
inline void __json(const __struct1 &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"n\":";
  __json(v.n);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"l\":";
  __json(v.l);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"b\":";
  __json(v.b);
  std::cout << "}";
}
int main() {
  std::vector<int> nums = std::vector<decltype(1)>{1, 2};
  std::vector<std::string> letters = std::vector<decltype(std::string("A"))>{
      std::string("A"), std::string("B")};
  std::vector<bool> bools = std::vector<decltype(true)>{true, false};
  auto combos = ([&]() {
    std::vector<__struct1> __items;
    for (auto n : nums) {
      for (auto l : letters) {
        for (auto b : bools) {
          __items.push_back(__struct1{n, l, b});
        }
      }
    }
    return __items;
  })();
  {
    std::cout << std::boolalpha
              << std::string("--- Cross Join of three lists ---");
    std::cout << std::endl;
  }
  for (auto c : combos) {
    {
      std::cout << std::boolalpha << c.n;
      std::cout << ' ';
      std::cout << std::boolalpha << c.l;
      std::cout << ' ';
      std::cout << std::boolalpha << c.b;
      std::cout << std::endl;
    }
  }
  return 0;
}
