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
  decltype(std::declval<__struct2>().id) orderId;
  decltype(std::declval<__struct1>().name) customerName;
  decltype(std::declval<__struct2>().total) total;
};
inline bool operator==(const __struct3 &a, const __struct3 &b) {
  return a.orderId == b.orderId && a.customerName == b.customerName &&
         a.total == b.total;
}
inline bool operator!=(const __struct3 &a, const __struct3 &b) {
  return !(a == b);
}
std::vector<__struct1> customers =
    std::vector<decltype(__struct1{1, std::string("Alice")})>{
        __struct1{1, std::string("Alice")}, __struct1{2, std::string("Bob")},
        __struct1{3, std::string("Charlie")}};
std::vector<__struct2> orders = std::vector<decltype(__struct2{100, 1, 250})>{
    __struct2{100, 1, 250}, __struct2{101, 2, 125}, __struct2{102, 1, 300},
    __struct2{103, 4, 80}};
auto result = ([]() {
  std::vector<__struct3> __items;
  for (auto o : orders) {
    for (auto c : customers) {
      if (!((o.customerId == c.id)))
        continue;
      __items.push_back(__struct3{o.id, c.name, o.total});
    }
  }
  return __items;
})();

int main() {
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
