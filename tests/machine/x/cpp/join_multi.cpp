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
  decltype(100) orderId;
  decltype(std::string("a")) sku;
};
struct __struct4 {
  decltype(std::declval<__struct1>().name) name;
  decltype(std::declval<__struct3>().sku) sku;
};
int main() {
  std::vector<__struct1> customers =
      std::vector<decltype(__struct1{1, std::string("Alice")})>{
          __struct1{1, std::string("Alice")}, __struct1{2, std::string("Bob")}};
  std::vector<__struct2> orders = std::vector<decltype(__struct2{100, 1})>{
      __struct2{100, 1}, __struct2{101, 2}};
  std::vector<__struct3> items =
      std::vector<decltype(__struct3{100, std::string("a")})>{
          __struct3{100, std::string("a")}, __struct3{101, std::string("b")}};
  auto result = ([&]() {
    std::vector<__struct4> __items;
    for (auto o : orders) {
      for (auto c : customers) {
        if (!((o.customerId == c.id)))
          continue;
        for (auto i : items) {
          if (!((o.id == i.orderId)))
            continue;
          __items.push_back(__struct4{c.name, i.sku});
        }
      }
    }
    return __items;
  })();
  {
    std::cout << std::boolalpha << std::string("--- Multi Join ---");
    std::cout << std::endl;
  }
  for (auto r : result) {
    {
      std::cout << std::boolalpha << r.name;
      std::cout << ' ';
      std::cout << std::boolalpha << std::string("bought item");
      std::cout << ' ';
      std::cout << std::boolalpha << r.sku;
      std::cout << std::endl;
    }
  }
  return 0;
}
