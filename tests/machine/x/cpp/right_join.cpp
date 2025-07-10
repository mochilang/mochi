#include <iostream>
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
  decltype(std::declval<__struct1>().name) customerName;
  __struct2 order;
};
inline bool operator==(const __struct3 &a, const __struct3 &b) {
  return a.customerName == b.customerName && a.order == b.order;
}
inline bool operator!=(const __struct3 &a, const __struct3 &b) {
  return !(a == b);
}
std::vector<__struct1> customers =
    std::vector<decltype(__struct1{1, std::string("Alice")})>{
        __struct1{1, std::string("Alice")}, __struct1{2, std::string("Bob")},
        __struct1{3, std::string("Charlie")},
        __struct1{4, std::string("Diana")}};
std::vector<__struct2> orders = std::vector<decltype(__struct2{100, 1, 250})>{
    __struct2{100, 1, 250}, __struct2{101, 2, 125}, __struct2{102, 1, 300}};
auto result = ([]() {
  std::vector<__struct3> __items;
  for (auto c : customers) {
    {
      bool __matched0 = false;
      for (auto o : orders) {
        if (!((o.customerId == c.id)))
          continue;
        __matched0 = true;
        __items.push_back(__struct3{c.name, o});
      }
      if (!__matched0) {
        auto o = std::decay_t<decltype(*(orders).begin())>{};
        __items.push_back(__struct3{c.name, o});
      }
    }
  }
  return __items;
})();

int main() {
  {
    std::cout << std::boolalpha
              << std::string("--- Right Join using syntax ---");
    std::cout << std::endl;
  }
  for (auto entry : result) {
    if ((entry.order != __struct2{})) {
      {
        std::cout << std::boolalpha << std::string("Customer");
        std::cout << ' ';
        std::cout << std::boolalpha << entry.customerName;
        std::cout << ' ';
        std::cout << std::boolalpha << std::string("has order");
        std::cout << ' ';
        std::cout << std::boolalpha << entry.order.id;
        std::cout << ' ';
        std::cout << std::boolalpha << std::string("- $");
        std::cout << ' ';
        std::cout << std::boolalpha << entry.order.total;
        std::cout << std::endl;
      }
    } else {
      {
        std::cout << std::boolalpha << std::string("Customer");
        std::cout << ' ';
        std::cout << std::boolalpha << entry.customerName;
        std::cout << ' ';
        std::cout << std::boolalpha << std::string("has no orders");
        std::cout << std::endl;
      }
    }
  }
  return 0;
}
