// Generated by Mochi compiler v0.10.26 on 2006-01-02T15:04:05Z
#include <iostream>
#include <map>
#include <numeric>
#include <string>
#include <unordered_map>
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

int main() {
  std::vector<std::string> store_sales =
      std::vector<decltype(std::unordered_map<std::string, decltype(40)>{
          {std::string("price"), 40}})>{
          std::unordered_map<std::string, decltype(40)>{
              {std::string("price"), 40}},
          std::unordered_map<std::string, decltype(30)>{
              {std::string("price"), 30}},
          std::unordered_map<std::string, decltype(19)>{
              {std::string("price"), 19}}};
  auto result = ([&](auto v) {
    return std::accumulate(v.begin(), v.end(), 0.0);
  })(([&]() {
    std::vector<decltype(s.price)> __items;
    for (auto s : store_sales) {
      __items.push_back(s.price);
    }
    return __items;
  })());
  (__json(result));
  // test TPCDS Q89 sample
  return 0;
}
