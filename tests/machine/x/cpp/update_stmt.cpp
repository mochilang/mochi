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
  decltype(std::string("Alice")) name;
  decltype(17) age;
  decltype(std::string("minor")) status;
};
inline void __json(const __struct1 &v) {
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
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"status\":";
  __json(v.status);
  std::cout << "}";
}
int main() {
  struct Person {
    std::string name;
    int age;
    std::string status;
  };
  auto people = std::vector<decltype(__struct1{std::string("Alice"), 17,
                                               std::string("minor")})>{
      __struct1{std::string("Alice"), 17, std::string("minor")},
      __struct1{std::string("Bob"), 25, std::string("unknown")},
      __struct1{std::string("Charlie"), 18, std::string("unknown")},
      __struct1{std::string("Diana"), 16, std::string("minor")}};
  for (auto &__tmp1 : people) {
    if ((__tmp1.age >= 18)) {
      __tmp1.status = std::string("adult");
      __tmp1.age = (__tmp1.age + 1);
    }
  }
  // test update adult status
  {
    std::cout << std::boolalpha << std::string("ok");
    std::cout << std::endl;
  }
  return 0;
}
