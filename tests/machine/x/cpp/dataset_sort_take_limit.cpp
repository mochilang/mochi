#include <algorithm>
#include <iostream>
#include <map>
#include <numeric>
#include <unordered_map>
#include <utility>
#include <vector>

template <typename T> void print_val(const T &v) { std::cout << v; }
void print_val(const std::vector<int> &v) {
  for (size_t i = 0; i < v.size(); ++i) {
    if (i)
      std::cout << ' ';
    std::cout << v[i];
  }
}
void print_val(bool b) { std::cout << (b ? "true" : "false"); }
void print() { std::cout << std::endl; }
template <typename First, typename... Rest>
void print(const First &first, const Rest &...rest) {
  print_val(first);
  if constexpr (sizeof...(rest) > 0) {
    std::cout << ' ';
    print(rest...);
  } else {
    std::cout << std::endl;
  }
}

struct __struct1 {
  decltype(std::string("Laptop")) name;
  decltype(1500) price;
};
int main() {
  auto products = std::vector<decltype(__struct1{std::string("Laptop"), 1500})>{
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
  print(std::string("--- Top products (excluding most expensive) ---"));
  for (auto item : expensive) {
    print(item.name, std::string("costs $"), item.price);
  }
  return 0;
}
