#include <iostream>
#include <map>
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

struct __struct1 {
  decltype(std::string("Alice")) name;
  decltype(std::string("Paris")) city;
};
inline bool operator==(const __struct1 &a, const __struct1 &b) {
  return a.name == b.name && a.city == b.city;
}
inline bool operator!=(const __struct1 &a, const __struct1 &b) {
  return !(a == b);
}
struct __struct2 {
  decltype(std::declval<__struct1>().city) key;
  std::vector<__struct1> items;
};
inline bool operator==(const __struct2 &a, const __struct2 &b) {
  return a.key == b.key && a.items == b.items;
}
inline bool operator!=(const __struct2 &a, const __struct2 &b) {
  return !(a == b);
}
struct __struct3 {
  decltype(std::declval<__struct2>().key) city;
  int num;
};
inline bool operator==(const __struct3 &a, const __struct3 &b) {
  return a.city == b.city && a.num == b.num;
}
inline bool operator!=(const __struct3 &a, const __struct3 &b) {
  return !(a == b);
}
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
  std::cout << "\"city\":";
  __json(v.city);
  std::cout << "}";
}
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
  std::cout << "\"num\":";
  __json(v.num);
  std::cout << "}";
}
std::vector<__struct1> people = std::vector<decltype(__struct1{
    std::string("Alice"), std::string("Paris")})>{
    __struct1{std::string("Alice"), std::string("Paris")},
    __struct1{std::string("Bob"), std::string("Hanoi")},
    __struct1{std::string("Charlie"), std::string("Paris")},
    __struct1{std::string("Diana"), std::string("Hanoi")},
    __struct1{std::string("Eve"), std::string("Paris")},
    __struct1{std::string("Frank"), std::string("Hanoi")},
    __struct1{std::string("George"), std::string("Paris")}};
auto big = ([]() {
  std::vector<__struct2> __groups;
  for (auto p : people) {
    auto __key = p.city;
    bool __found = false;
    for (auto &__g : __groups) {
      if (__g.key == __key) {
        __g.items.push_back(__struct1{p});
        __found = true;
        break;
      }
    }
    if (!__found) {
      __groups.push_back(
          __struct2{__key, std::vector<__struct1>{__struct1{p}}});
    }
  }
  std::vector<__struct3> __items;
  for (auto &g : __groups) {
    if (!((((int)g.items.size()) >= 4)))
      continue;
    __items.push_back(__struct3{g.key, ((int)g.items.size())});
  }
  return __items;
})();

int main() {
  (__json(big));
  return 0;
}
