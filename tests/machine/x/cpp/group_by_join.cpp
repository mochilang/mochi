#include <algorithm>
#include <iostream>
#include <map>
#include <numeric>
#include <unordered_map>
#include <utility>
#include <vector>

struct __struct1 {
  decltype(1) id;
  decltype(std::string("Alice")) name;
};
struct __struct2 {
  decltype(100) id;
  decltype(1) customerId;
};
struct __struct3 {
  __struct2 o;
  __struct1 c;
};
struct __struct4 {
  decltype(std::declval<__struct1>().name) key;
  std::vector<__struct3> items;
};
struct __struct5 {
  decltype(std::declval<__struct4>().key) name;
  int count;
};
int main() {
  std::vector<__struct1> customers =
      std::vector<decltype(__struct1{1, std::string("Alice")})>{
          __struct1{1, std::string("Alice")}, __struct1{2, std::string("Bob")}};
  std::vector<__struct2> orders = std::vector<decltype(__struct2{100, 1})>{
      __struct2{100, 1}, __struct2{101, 1}, __struct2{102, 2}};
  auto stats = ([&]() {
    std::vector<__struct4> __groups;
    for (auto o : orders) {
      for (auto c : customers) {
        if (!((o.customerId == c.id)))
          continue;
        auto __key = c.name;
        bool __found = false;
        for (auto &__g : __groups) {
          if (__g.key == __key) {
            __g.items.push_back(__struct3{o, c});
            __found = true;
            break;
          }
        }
        if (!__found) {
          __groups.push_back(
              __struct4{__key, std::vector<__struct3>{__struct3{o, c}}});
        }
      }
    }
    std::vector<__struct5> __items;
    for (auto &g : __groups) {
      __items.push_back(__struct5{g.key, ((int)g.items.size())});
    }
    return __items;
  })();
  {
    std::cout << std::boolalpha << std::string("--- Orders per customer ---");
    std::cout << std::endl;
  }
  for (auto s : stats) {
    {
      std::cout << std::boolalpha << s.name;
      std::cout << ' ';
      std::cout << std::boolalpha << std::string("orders:");
      std::cout << ' ';
      std::cout << std::boolalpha << s.count;
      std::cout << std::endl;
    }
  }
  return 0;
}
