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
  decltype(std::declval<Order>().customerId) orderCustomerId;
  decltype(std::declval<Customer>().name) pairedCustomerName;
  decltype(std::declval<Order>().total) orderTotal;
};
inline bool operator==(const Result &a, const Result &b) {
  return a.orderId == b.orderId && a.orderCustomerId == b.orderCustomerId &&
         a.pairedCustomerName == b.pairedCustomerName &&
         a.orderTotal == b.orderTotal;
}
inline bool operator!=(const Result &a, const Result &b) { return !(a == b); }
int main() {
  std::vector<Customer> customers = std::vector<Customer>{
      Customer{1, std::string("Alice")}, Customer{2, std::string("Bob")},
      Customer{3, std::string("Charlie")}};
  std::vector<Order> orders = std::vector<Order>{
      Order{100, 1, 250}, Order{101, 2, 125}, Order{102, 1, 300}};
  auto result = ([&]() {
    std::vector<Result> __items;
    for (auto o : orders) {
      for (auto c : customers) {
        __items.push_back(Result{o.id, o.customerId, c.name, o.total});
      }
    }
    return __items;
  })();
  std::cout << std::string("--- Cross Join: All order-customer pairs ---")
            << std::endl;
  for (auto entry : result) {
    {
      std::cout << std::string("Order");
      std::cout << ' ';
      std::cout << std::boolalpha << entry.orderId;
      std::cout << ' ';
      std::cout << std::string("(customerId:");
      std::cout << ' ';
      std::cout << std::boolalpha << entry.orderCustomerId;
      std::cout << ' ';
      std::cout << std::string(", total: $");
      std::cout << ' ';
      std::cout << std::boolalpha << entry.orderTotal;
      std::cout << ' ';
      std::cout << std::string(") paired with");
      std::cout << ' ';
      std::cout << std::boolalpha << entry.pairedCustomerName;
      std::cout << std::endl;
    }
  }
  return 0;
}
