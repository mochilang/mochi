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

auto classify(auto n) {
  return ([&]() {
    auto __v = n;
    if (__v == 0)
      return std::string("zero");
    else if (__v == 1)
      return std::string("one");
    return std::string("many");
  })();
}

int main() {
  auto x = 2;
  auto label = ([&]() {
    auto __v = x;
    if (__v == 1)
      return std::string("one");
    else if (__v == 2)
      return std::string("two");
    else if (__v == 3)
      return std::string("three");
    return std::string("unknown");
  })();
  {
    std::cout << std::boolalpha << label;
    std::cout << std::endl;
  }
  auto day = std::string("sun");
  auto mood = ([&]() {
    auto __v = day;
    if (__v == std::string("mon"))
      return std::string("tired");
    else if (__v == std::string("fri"))
      return std::string("excited");
    else if (__v == std::string("sun"))
      return std::string("relaxed");
    return std::string("normal");
  })();
  {
    std::cout << std::boolalpha << mood;
    std::cout << std::endl;
  }
  auto ok = true;
  auto status = ([&]() {
    auto __v = ok;
    if (__v == true)
      return std::string("confirmed");
    else if (__v == false)
      return std::string("denied");
    return decltype(std::string("confirmed")){};
  })();
  {
    std::cout << std::boolalpha << status;
    std::cout << std::endl;
  }
  {
    std::cout << std::boolalpha << classify(0);
    std::cout << std::endl;
  }
  {
    std::cout << std::boolalpha << classify(5);
    std::cout << std::endl;
  }
  return 0;
}
