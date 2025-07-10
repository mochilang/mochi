#include <iostream>
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
  decltype(std::declval<__struct2>().id) orderId;
  decltype(std::declval<__struct1>().name) name;
  decltype(i) item;
};
std::vector<__struct1> customers =
    std::vector<decltype(__struct1{1, std::string("Alice")})>{
        __struct1{1, std::string("Alice")}, __struct1{2, std::string("Bob")}};
std::vector<__struct2> orders = std::vector<decltype(__struct2{100, 1})>{
    __struct2{100, 1}, __struct2{101, 2}};
std::vector<__struct3> items =
    std::vector<decltype(__struct3{100, std::string("a")})>{
        __struct3{100, std::string("a")}};
auto result = ([]() {
  std::vector<__struct4> __items;
  for (auto o : orders) {
    for (auto c : customers) {
      if (!((o.customerId == c.id)))
        continue;
      {
        bool __matched1 = false;
        for (auto i : items) {
          if (!((o.id == i.orderId)))
            continue;
          __matched1 = true;
          __items.push_back(__struct4{o.id, c.name, i});
        }
        if (!__matched1) {
          auto i = std::decay_t<decltype(*(items).begin())>{};
          __items.push_back(__struct4{o.id, c.name, i});
        }
      }
    }
  }
  return __items;
})();

int main() {
  {
    std::cout << std::boolalpha << std::string("--- Left Join Multi ---");
    std::cout << std::endl;
  }
  for (auto r : result) {
    {
      std::cout << std::boolalpha << r.orderId;
      std::cout << ' ';
      std::cout << std::boolalpha << r.name;
      std::cout << ' ';
      std::cout << std::boolalpha << r.item;
      std::cout << std::endl;
    }
  }
  return 0;
}
