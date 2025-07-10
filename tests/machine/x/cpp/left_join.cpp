#include <iostream>
#include <vector>

struct __struct1 {
  decltype(1) id;
  decltype(std::string("Alice")) name;
};
struct __struct2 {
  decltype(100) id;
  decltype(1) customerId;
  decltype(250) total;
};
struct __struct3 {
  decltype(std::declval<__struct2>().id) orderId;
  decltype(c) customer;
  decltype(std::declval<__struct2>().total) total;
};
int main() {
  std::vector<__struct1> customers =
      std::vector<decltype(__struct1{1, std::string("Alice")})>{
          __struct1{1, std::string("Alice")}, __struct1{2, std::string("Bob")}};
  std::vector<__struct2> orders = std::vector<decltype(__struct2{100, 1, 250})>{
      __struct2{100, 1, 250}, __struct2{101, 3, 80}};
  auto result = ([&]() {
    std::vector<__struct3> __items;
    for (auto o : orders) {
      {
        bool __matched0 = false;
        for (auto c : customers) {
          if (!((o.customerId == c.id)))
            continue;
          __matched0 = true;
          __items.push_back(__struct3{o.id, c, o.total});
        }
        if (!__matched0) {
          auto c = std::decay_t<decltype(*(customers).begin())>{};
          __items.push_back(__struct3{o.id, c, o.total});
        }
      }
    }
    return __items;
  })();
  {
    std::cout << std::boolalpha << std::string("--- Left Join ---");
    std::cout << std::endl;
  }
  for (auto entry : result) {
    {
      std::cout << std::boolalpha << std::string("Order");
      std::cout << ' ';
      std::cout << std::boolalpha << entry.orderId;
      std::cout << ' ';
      std::cout << std::boolalpha << std::string("customer");
      std::cout << ' ';
      std::cout << std::boolalpha << entry.customer;
      std::cout << ' ';
      std::cout << std::boolalpha << std::string("total");
      std::cout << ' ';
      std::cout << std::boolalpha << entry.total;
      std::cout << std::endl;
    }
  }
  return 0;
}
