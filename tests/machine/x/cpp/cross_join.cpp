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
  decltype(std::declval<__struct2>().customerId) orderCustomerId;
  decltype(std::declval<__struct1>().name) pairedCustomerName;
  decltype(std::declval<__struct2>().total) orderTotal;
};
int main() {
  std::vector<__struct1> customers =
      std::vector<decltype(__struct1{1, std::string("Alice")})>{
          __struct1{1, std::string("Alice")}, __struct1{2, std::string("Bob")},
          __struct1{3, std::string("Charlie")}};
  std::vector<__struct2> orders = std::vector<decltype(__struct2{100, 1, 250})>{
      __struct2{100, 1, 250}, __struct2{101, 2, 125}, __struct2{102, 1, 300}};
  auto result = ([&]() {
    std::vector<__struct3> __items;
    for (auto o : orders) {
      for (auto c : customers) {
        __items.push_back(__struct3{o.id, o.customerId, c.name, o.total});
      }
    }
    return __items;
  })();
  {
    std::cout << std::boolalpha
              << std::string("--- Cross Join: All order-customer pairs ---");
    std::cout << std::endl;
  }
  for (auto entry : result) {
    {
      std::cout << std::boolalpha << std::string("Order");
      std::cout << ' ';
      std::cout << std::boolalpha << entry.orderId;
      std::cout << ' ';
      std::cout << std::boolalpha << std::string("(customerId:");
      std::cout << ' ';
      std::cout << std::boolalpha << entry.orderCustomerId;
      std::cout << ' ';
      std::cout << std::boolalpha << std::string(", total: $");
      std::cout << ' ';
      std::cout << std::boolalpha << entry.orderTotal;
      std::cout << ' ';
      std::cout << std::boolalpha << std::string(") paired with");
      std::cout << ' ';
      std::cout << std::boolalpha << entry.pairedCustomerName;
      std::cout << std::endl;
    }
  }
  return 0;
}
