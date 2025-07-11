#include <algorithm>
#include <iostream>
#include <string>
#include <utility>
#include <vector>

struct Product {
  decltype(std::string("Laptop")) name;
  decltype(1500) price;
};
inline bool operator==(const Product &a, const Product &b) {
  return a.name == b.name && a.price == b.price;
}
inline bool operator!=(const Product &a, const Product &b) { return !(a == b); }
int main() {
  std::vector<Product> products =
      std::vector<Product>{Product{std::string("Laptop"), 1500},
                           Product{std::string("Smartphone"), 900},
                           Product{std::string("Tablet"), 600},
                           Product{std::string("Monitor"), 300},
                           Product{std::string("Keyboard"), 100},
                           Product{std::string("Mouse"), 50},
                           Product{std::string("Headphones"), 200}};
  auto expensive = ([&]() {
    std::vector<std::pair<decltype(std::declval<Product>().price), Product>>
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
    std::vector<Product> __res;
    for (auto &p : __items)
      __res.push_back(p.second);
    return __res;
  })();
  std::cout << std::string("--- Top products (excluding most expensive) ---")
            << std::endl;
  for (auto item : expensive) {
    {
      std::cout << std::boolalpha << item.name;
      std::cout << ' ';
      std::cout << std::string("costs $");
      std::cout << ' ';
      std::cout << std::boolalpha << item.price;
      std::cout << std::endl;
    }
  }
  return 0;
}
