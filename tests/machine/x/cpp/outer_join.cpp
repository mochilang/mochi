#include <iostream>
#include <string>
#include <vector>

struct __struct1 {
  decltype(1) id;
  decltype(std::string("Alice")) name;
};
inline bool operator==(const __struct1 &a, const __struct1 &b) {
  return a.id == b.id && a.name == b.name;
}
inline bool operator!=(const __struct1 &a, const __struct1 &b) {
  return !(a == b);
}
struct __struct2 {
  decltype(100) id;
  decltype(1) customerId;
  decltype(250) total;
};
inline bool operator==(const __struct2 &a, const __struct2 &b) {
  return a.id == b.id && a.customerId == b.customerId && a.total == b.total;
}
inline bool operator!=(const __struct2 &a, const __struct2 &b) {
  return !(a == b);
}
struct __struct3 {
  decltype(o) order;
  decltype(c) customer;
};
inline bool operator==(const __struct3 &a, const __struct3 &b) {
  return a.order == b.order && a.customer == b.customer;
}
inline bool operator!=(const __struct3 &a, const __struct3 &b) {
  return !(a == b);
}
int main() {
  auto customers = std::vector<__struct1>{
      __struct1{1, std::string("Alice")}, __struct1{2, std::string("Bob")},
      __struct1{3, std::string("Charlie")}, __struct1{4, std::string("Diana")}};
  auto orders =
      std::vector<__struct2>{__struct2{100, 1, 250}, __struct2{101, 2, 125},
                             __struct2{102, 1, 300}, __struct2{103, 5, 80}};
  auto result = ([&]() {
    std::vector<__struct3> __items;
    for (auto o : orders) {
      {
        bool __matched0 = false;
        for (auto c : customers) {
          if (!((o.customerId == c.id)))
            continue;
          __matched0 = true;
          __items.push_back(__struct3{o, c});
        }
        if (!__matched0) {
          auto c = std::decay_t<decltype(*(customers).begin())>{};
          __items.push_back(__struct3{o, c});
        }
      }
    }
    return __items;
  })();
  std::cout << std::string("--- Outer Join using syntax ---") << std::endl;
  for (auto row : result) {
    if (row.order) {
      if (row.customer) {
        {
          std::cout << std::string("Order");
          std::cout << ' ';
          std::cout << std::boolalpha << row.order.id;
          std::cout << ' ';
          std::cout << std::string("by");
          std::cout << ' ';
          std::cout << std::boolalpha << row.customer.name;
          std::cout << ' ';
          std::cout << std::string("- $");
          std::cout << ' ';
          std::cout << std::boolalpha << row.order.total;
          std::cout << std::endl;
        }
      } else {
        {
          std::cout << std::string("Order");
          std::cout << ' ';
          std::cout << std::boolalpha << row.order.id;
          std::cout << ' ';
          std::cout << std::string("by");
          std::cout << ' ';
          std::cout << std::string("Unknown");
          std::cout << ' ';
          std::cout << std::string("- $");
          std::cout << ' ';
          std::cout << std::boolalpha << row.order.total;
          std::cout << std::endl;
        }
      }
    } else {
      {
        std::cout << std::string("Customer");
        std::cout << ' ';
        std::cout << std::boolalpha << row.customer.name;
        std::cout << ' ';
        std::cout << std::string("has no orders");
        std::cout << std::endl;
      }
    }
  }
  return 0;
}
