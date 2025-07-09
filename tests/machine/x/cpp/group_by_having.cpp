#include <algorithm>
#include <iostream>
#include <map>
#include <numeric>
#include <unordered_map>
#include <utility>
#include <vector>

struct __struct1 {
  decltype(std::string("Alice")) name;
  decltype(std::string("Paris")) city;
};
struct __struct2 {
  decltype(std::declval<__struct1>().city) key;
  std::vector<__struct1> items;
};
struct __struct3 {
  decltype(std::declval<__struct2>().key) city;
  int num;
};
int main() {
  std::vector<__struct1> people = std::vector<decltype(__struct1{
      std::string("Alice"), std::string("Paris")})>{
      __struct1{std::string("Alice"), std::string("Paris")},
      __struct1{std::string("Bob"), std::string("Hanoi")},
      __struct1{std::string("Charlie"), std::string("Paris")},
      __struct1{std::string("Diana"), std::string("Hanoi")},
      __struct1{std::string("Eve"), std::string("Paris")},
      __struct1{std::string("Frank"), std::string("Hanoi")},
      __struct1{std::string("George"), std::string("Paris")}};
  auto big = ([&]() {
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
  json(big);
  return 0;
}
