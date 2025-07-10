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
  decltype(std::string("Paris")) city;
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
  std::cout << "\"city\":";
  __json(v.city);
  std::cout << "}";
}
struct __struct2 {
  decltype(std::declval<__struct1>().city) key;
  std::vector<__struct1> items;
};
struct __struct3 {
  decltype(std::declval<__struct2>().key) city;
  int count;
  bool avg_age;
};
inline void __json(const __struct3 &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"city\":";
  __json(v.city);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"count\":";
  __json(v.count);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"avg_age\":";
  __json(v.avg_age);
  std::cout << "}";
}
int main() {
  std::vector<__struct1> people = std::vector<decltype(__struct1{
      std::string("Alice"), 30, std::string("Paris")})>{
      __struct1{std::string("Alice"), 30, std::string("Paris")},
      __struct1{std::string("Bob"), 15, std::string("Hanoi")},
      __struct1{std::string("Charlie"), 65, std::string("Paris")},
      __struct1{std::string("Diana"), 45, std::string("Hanoi")},
      __struct1{std::string("Eve"), 70, std::string("Paris")},
      __struct1{std::string("Frank"), 22, std::string("Hanoi")}};
  auto stats = ([&]() {
    std::vector<__struct2> __groups;
    for (auto person : people) {
      auto __key = person.city;
      bool __found = false;
      for (auto &__g : __groups) {
        if (__g.key == __key) {
          __g.items.push_back(__struct1{person});
          __found = true;
          break;
        }
      }
      if (!__found) {
        __groups.push_back(
            __struct2{__key, std::vector<__struct1>{__struct1{person}}});
      }
    }
    std::vector<__struct3> __items;
    for (auto &g : __groups) {
      __items.push_back(__struct3{
          g.key, ((int)g.items.size()), ([&](auto v) {
            int s = 0;
            for (auto x : v)
              s += x;
            return v.empty() ? 0 : (double)s / v.size();
          })(([&]() {
            std::vector<decltype(std::declval<__struct1>().age)> __items;
            for (auto p : g.items) {
              __items.push_back(p.age);
            }
            return __items;
          })())});
    }
    return __items;
  })();
  {
    std::cout << std::boolalpha
              << std::string("--- People grouped by city ---");
    std::cout << std::endl;
  }
  for (auto s : stats) {
    {
      std::cout << std::boolalpha << s.city;
      std::cout << ' ';
      std::cout << std::boolalpha << std::string(": count =");
      std::cout << ' ';
      std::cout << std::boolalpha << s.count;
      std::cout << ' ';
      std::cout << std::boolalpha << std::string(", avg_age =");
      std::cout << ' ';
      std::cout << std::boolalpha << s.avg_age;
      std::cout << std::endl;
    }
  }
  return 0;
}
