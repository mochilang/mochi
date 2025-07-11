#include <iostream>
#include <string>
#include <vector>

struct Customer {
  decltype(1) id;
  decltype(std::string("Alice")) name;
};
inline bool operator==(const Customer &a, const Customer &b) {
  return a.id == b.id && a.name == b.name;
}
inline bool operator!=(const Customer &a, const Customer &b) {
  return !(a == b);
}
struct Order {
  decltype(100) id;
  decltype(1) customerId;
  decltype(250) total;
};
inline bool operator==(const Order &a, const Order &b) {
  return a.id == b.id && a.customerId == b.customerId && a.total == b.total;
}
inline bool operator!=(const Order &a, const Order &b) { return !(a == b); }
struct Result {
  decltype(std::declval<Order>().id) orderId;
  decltype(std::declval<Customer>().name) customerName;
  decltype(std::declval<Order>().total) total;
};
inline bool operator==(const Result &a, const Result &b) {
  return a.orderId == b.orderId && a.customerName == b.customerName &&
         a.total == b.total;
}
inline bool operator!=(const Result &a, const Result &b) { return !(a == b); }
int main() {
  std::vector<Customer> customers = std::vector<Customer>{
      Customer{1, std::string("Alice")}, Customer{2, std::string("Bob")},
      Customer{3, std::string("Charlie")}};
  std::vector<Order> orders =
      std::vector<Order>{Order{100, 1, 250}, Order{101, 2, 125},
                         Order{102, 1, 300}, Order{103, 4, 80}};
  auto result = ([&]() {
    std::vector<Result> __items;
    for (auto o : orders) {
      for (auto c : customers) {
        if (!((o.customerId == c.id)))
          continue;
        __items.push_back(Result{o.id, c.name, o.total});
      }
    }
    return __items;
  })();
  std::cout << std::string("--- Orders with customer info ---") << std::endl;
  for (auto entry : result) {
    {
      std::cout << std::string("Order");
      std::cout << ' ';
      std::cout << std::boolalpha << entry.orderId;
      std::cout << ' ';
      std::cout << std::string("by");
      std::cout << ' ';
      std::cout << std::boolalpha << entry.customerName;
      std::cout << ' ';
      std::cout << std::string("- $");
      std::cout << ' ';
      std::cout << std::boolalpha << entry.total;
      std::cout << std::endl;
    }
  }
  return 0;
}
