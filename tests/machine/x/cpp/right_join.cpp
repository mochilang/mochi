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
  decltype(std::declval<Customer>().name) customerName;
  Order order;
};
inline bool operator==(const Result &a, const Result &b) {
  return a.customerName == b.customerName && a.order == b.order;
}
inline bool operator!=(const Result &a, const Result &b) { return !(a == b); }
int main() {
  std::vector<Customer> customers = std::vector<Customer>{
      Customer{1, std::string("Alice")}, Customer{2, std::string("Bob")},
      Customer{3, std::string("Charlie")}, Customer{4, std::string("Diana")}};
  std::vector<Order> orders = std::vector<Order>{
      Order{100, 1, 250}, Order{101, 2, 125}, Order{102, 1, 300}};
  auto result = ([&]() {
    std::vector<Result> __items;
    for (auto c : customers) {
      {
        bool __matched0 = false;
        for (auto o : orders) {
          if (!((o.customerId == c.id)))
            continue;
          __matched0 = true;
          __items.push_back(Result{c.name, o});
        }
        if (!__matched0) {
          auto o = std::decay_t<decltype(*(orders).begin())>{};
          __items.push_back(Result{c.name, o});
        }
      }
    }
    return __items;
  })();
  std::cout << std::string("--- Right Join using syntax ---") << std::endl;
  for (auto entry : result) {
    if ((entry.order != Order{})) {
      {
        std::cout << std::string("Customer");
        std::cout << ' ';
        std::cout << std::boolalpha << entry.customerName;
        std::cout << ' ';
        std::cout << std::string("has order");
        std::cout << ' ';
        std::cout << std::boolalpha << entry.order.id;
        std::cout << ' ';
        std::cout << std::string("- $");
        std::cout << ' ';
        std::cout << std::boolalpha << entry.order.total;
        std::cout << std::endl;
      }
    } else {
      {
        std::cout << std::string("Customer");
        std::cout << ' ';
        std::cout << std::boolalpha << entry.customerName;
        std::cout << ' ';
        std::cout << std::string("has no orders");
        std::cout << std::endl;
      }
    }
  }
  return 0;
}
