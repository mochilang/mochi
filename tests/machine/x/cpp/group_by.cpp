#include <iostream>
#include <string>
#include <vector>

struct __struct1 {
  decltype(std::string("Alice")) name;
  decltype(30) age;
  decltype(std::string("Paris")) city;
};
inline bool operator==(const __struct1 &a, const __struct1 &b) {
  return a.name == b.name && a.age == b.age && a.city == b.city;
}
inline bool operator!=(const __struct1 &a, const __struct1 &b) {
  return !(a == b);
}
struct __struct2 {
  decltype(person) person;
};
inline bool operator==(const __struct2 &a, const __struct2 &b) {
  return a.person == b.person;
}
inline bool operator!=(const __struct2 &a, const __struct2 &b) {
  return !(a == b);
}
struct __struct3 {
  decltype(person.city) key;
  std::vector<__struct2> items;
};
inline bool operator==(const __struct3 &a, const __struct3 &b) {
  return a.key == b.key && a.items == b.items;
}
inline bool operator!=(const __struct3 &a, const __struct3 &b) {
  return !(a == b);
}
template <typename T> double __avg(const std::vector<T> &v) {
  if (v.empty())
    return 0;
  double s = 0;
  for (const auto &x : v)
    s += x;
  return s / v.size();
}
struct __struct4 {
  decltype(std::declval<__struct3>().key) city;
  int count;
  bool avg_age;
};
inline bool operator==(const __struct4 &a, const __struct4 &b) {
  return a.city == b.city && a.count == b.count && a.avg_age == b.avg_age;
}
inline bool operator!=(const __struct4 &a, const __struct4 &b) {
  return !(a == b);
}
int main() {
  auto people = std::vector<__struct1>{
      __struct1{std::string("Alice"), 30, std::string("Paris")},
      __struct1{std::string("Bob"), 15, std::string("Hanoi")},
      __struct1{std::string("Charlie"), 65, std::string("Paris")},
      __struct1{std::string("Diana"), 45, std::string("Hanoi")},
      __struct1{std::string("Eve"), 70, std::string("Paris")},
      __struct1{std::string("Frank"), 22, std::string("Hanoi")}};
  auto stats = ([&]() {
    std::vector<__struct3> __groups;
    for (auto person : people) {
      auto __key = person.city;
      bool __found = false;
      for (auto &__g : __groups) {
        if (__g.key == __key) {
          __g.items.push_back(__struct2{person});
          __found = true;
          break;
        }
      }
      if (!__found) {
        __groups.push_back(
            __struct3{__key, std::vector<__struct2>{__struct2{person}}});
      }
    }
    std::vector<__struct4> __items;
    for (auto &g : __groups) {
      __items.push_back(__struct4{
          g.key, ((int)g.items.size()), __avg(([&]() {
            std::vector<decltype(std::declval<__struct2>().age)> __items;
            for (auto p : g.items) {
              __items.push_back(p.age);
            }
            return __items;
          })())});
    }
    return __items;
  })();
  std::cout << std::string("--- People grouped by city ---") << std::endl;
  for (auto s : stats) {
    {
      std::cout << std::boolalpha << s.city;
      std::cout << ' ';
      std::cout << std::string(": count =");
      std::cout << ' ';
      std::cout << std::boolalpha << s.count;
      std::cout << ' ';
      std::cout << std::string(", avg_age =");
      std::cout << ' ';
      std::cout << std::boolalpha << s.avg_age;
      std::cout << std::endl;
    }
  }
  return 0;
}
