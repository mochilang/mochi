#include <algorithm>
#include <iostream>
#include <utility>
#include <vector>

struct __struct1 {
  decltype(std::string("Laptop")) name;
  decltype(1500) price;
};
int main() {
  std::vector<__struct1> products =
      std::vector<decltype(__struct1{std::string("Laptop"), 1500})>{
          __struct1{std::string("Laptop"), 1500},
          __struct1{std::string("Smartphone"), 900},
          __struct1{std::string("Tablet"), 600},
          __struct1{std::string("Monitor"), 300},
          __struct1{std::string("Keyboard"), 100},
          __struct1{std::string("Mouse"), 50},
          __struct1{std::string("Headphones"), 200}};
  auto expensive = ([&]() {
    std::vector<std::pair<decltype(std::declval<__struct1>().price), __struct1>>
        __items;
    for (auto p : products) {
      __items.push_back({(-p.price), p});
    }
    std::sort(__items.begin(), __items.end(),
              [](auto &a, auto &b) { return a.first < b.first; });
    if ((size_t)1 < __items.size())
      __items.erase(__items.begin(), __items.begin() + 1);
    if ((size_t)3 < __items.size())
      __items.resize(3);
    std::vector<__struct1> __res;
    for (auto &p : __items)
      __res.push_back(p.second);
    return __res;
  })();
  {
    std::cout << std::boolalpha
              << std::string("--- Top products (excluding most expensive) ---");
    std::cout << std::endl;
  }
  for (auto item : expensive) {
    {
      std::cout << std::boolalpha << item.name;
      std::cout << ' ';
      std::cout << std::boolalpha << std::string("costs $");
      std::cout << ' ';
      std::cout << std::boolalpha << item.price;
      std::cout << std::endl;
    }
  }
  return 0;
}
