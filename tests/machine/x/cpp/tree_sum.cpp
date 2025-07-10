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
  decltype(Leaf) left;
  decltype(2) value;
  decltype(Leaf) right;
};
inline void __json(const __struct1 &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"left\":";
  __json(v.left);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"value\":";
  __json(v.value);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"right\":";
  __json(v.right);
  std::cout << "}";
}
auto sum_tree(auto t) {
  return ([&]() {
    auto __v = t;
    if (__v == Leaf)
      return 0;
    else if (__v == Node(left, value, right))
      return ((sum_tree(left) + value) + sum_tree(right));
    return decltype(0){};
  })();
}

int main() {
  auto t = __struct1{Leaf, 1, __struct1{Leaf, 2, Leaf}};
  {
    std::cout << std::boolalpha << sum_tree(t);
    std::cout << std::endl;
  }
  return 0;
}
