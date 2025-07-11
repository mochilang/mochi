#include <iostream>
#include <map>
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

struct People {
  decltype(std::string("Alice")) name;
  decltype(30) age;
};
inline bool operator==(const People &a, const People &b) {
  return a.name == b.name && a.age == b.age;
}
inline bool operator!=(const People &a, const People &b) { return !(a == b); }
inline void __json(const People &v) {
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
  std::cout << "\"age\":";
  __json(v.age);
  std::cout << "}";
}
int main() {
  std::vector<People> people = std::vector<People>{
      People{std::string("Alice"), 30}, People{std::string("Bob"), 25}};
  ([&]() {
    for (auto &x : people) {
      __json(x);
      std::cout << std::endl;
    }
  })();
  return 0;
}
