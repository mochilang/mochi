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
};
inline bool operator==(const Order &a, const Order &b) {
  return a.id == b.id && a.customerId == b.customerId;
}
inline bool operator!=(const Order &a, const Order &b) { return !(a == b); }
struct Item {
  decltype(100) orderId;
  decltype(std::string("a")) sku;
};
inline bool operator==(const Item &a, const Item &b) {
  return a.orderId == b.orderId && a.sku == b.sku;
}
inline bool operator!=(const Item &a, const Item &b) { return !(a == b); }
struct Result {
  decltype(std::declval<Customer>().name) name;
  decltype(std::declval<Item>().sku) sku;
};
inline bool operator==(const Result &a, const Result &b) {
  return a.name == b.name && a.sku == b.sku;
}
inline bool operator!=(const Result &a, const Result &b) { return !(a == b); }
int main() {
  std::vector<Customer> customers = std::vector<Customer>{
      Customer{1, std::string("Alice")}, Customer{2, std::string("Bob")}};
  std::vector<Order> orders = std::vector<Order>{Order{100, 1}, Order{101, 2}};
  std::vector<Item> items = std::vector<Item>{Item{100, std::string("a")},
                                              Item{101, std::string("b")}};
  auto result = ([&]() {
    std::vector<Result> __items;
    for (auto o : orders) {
      for (auto c : customers) {
        if (!((o.customerId == c.id)))
          continue;
        for (auto i : items) {
          if (!((o.id == i.orderId)))
            continue;
          __items.push_back(Result{c.name, i.sku});
        }
      }
    }
    return __items;
  })();
  std::cout << std::string("--- Multi Join ---") << std::endl;
  for (auto r : result) {
    {
      std::cout << std::boolalpha << r.name;
      std::cout << ' ';
      std::cout << std::string("bought item");
      std::cout << ' ';
      std::cout << std::boolalpha << r.sku;
      std::cout << std::endl;
    }
  }
  return 0;
}
