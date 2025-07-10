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

int main() {
  {
    auto __tmp1 = ([&](auto a, auto b) {
      a.insert(a.end(), b.begin(), b.end());
      std::sort(a.begin(), a.end());
      a.erase(std::unique(a.begin(), a.end()), a.end());
      return a;
    })(std::vector<decltype(1)>{1, 2}, std::vector<decltype(2)>{2, 3});
    for (size_t i = 0; i < __tmp1.size(); ++i) {
      if (i)
        std::cout << ' ';
      std::cout << std::boolalpha << __tmp1[i];
    }
    std::cout << std::endl;
  }
  {
    auto __tmp2 = ([&](auto a, auto b) {
      for (auto &x : b)
        a.erase(std::remove(a.begin(), a.end(), x), a.end());
      return a;
    })(std::vector<decltype(1)>{1, 2, 3}, std::vector<decltype(2)>{2});
    for (size_t i = 0; i < __tmp2.size(); ++i) {
      if (i)
        std::cout << ' ';
      std::cout << std::boolalpha << __tmp2[i];
    }
    std::cout << std::endl;
  }
  {
    auto __tmp3 = ([&](auto a, auto b) {
      std::vector<std::decay_t<decltype(a[0])>> r_;
      for (auto &x : a)
        if (std::find(b.begin(), b.end(), x) != b.end())
          r_.push_back(x);
      return r_;
    })(std::vector<decltype(1)>{1, 2, 3}, std::vector<decltype(2)>{2, 4});
    for (size_t i = 0; i < __tmp3.size(); ++i) {
      if (i)
        std::cout << ' ';
      std::cout << std::boolalpha << __tmp3[i];
    }
    std::cout << std::endl;
  }
  {
    std::cout << std::boolalpha
              << ([&](auto a, auto b) {
                   a.insert(a.end(), b.begin(), b.end());
                   std::sort(a.begin(), a.end());
                   a.erase(std::unique(a.begin(), a.end()), a.end());
                   return a;
                 })(std::vector<decltype(1)>{1, 2},
                    std::vector<decltype(2)>{2, 3})
                     .size();
    std::cout << std::endl;
  }
  return 0;
}
