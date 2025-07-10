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
  decltype(30) age;
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
  std::cout << "}";
}
struct __struct2 {
  decltype(std::declval<__struct1>().name) name;
  decltype(std::declval<__struct1>().age) age;
  bool is_senior;
};
inline void __json(const __struct2 &v) {
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
  std::cout << "\"is_senior\":";
  __json(v.is_senior);
  std::cout << "}";
}
int main() {
  std::vector<__struct1> people =
      std::vector<decltype(__struct1{std::string("Alice"), 30})>{
          __struct1{std::string("Alice"), 30},
          __struct1{std::string("Bob"), 15},
          __struct1{std::string("Charlie"), 65},
          __struct1{std::string("Diana"), 45}};
  auto adults = ([&]() {
    std::vector<__struct2> __items;
    for (auto person : people) {
      if (!((person.age >= 18)))
        continue;
      __items.push_back(__struct2{person.name, person.age, (person.age >= 60)});
    }
    return __items;
  })();
  {
    std::cout << std::boolalpha << std::string("--- Adults ---");
    std::cout << std::endl;
  }
  for (auto person : adults) {
    {
      std::cout << std::boolalpha << person.name;
      std::cout << ' ';
      std::cout << std::boolalpha << std::string("is");
      std::cout << ' ';
      std::cout << std::boolalpha << person.age;
      std::cout << ' ';
      std::cout << std::boolalpha
                << (person.is_senior ? std::string(" (senior)")
                                     : std::string(""));
      std::cout << std::endl;
    }
  }
  return 0;
}
