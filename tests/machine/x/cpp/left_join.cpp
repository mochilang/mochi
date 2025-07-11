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
  decltype(o.id) orderId;
  decltype(c) customer;
  decltype(o.total) total;
};
inline bool operator==(const __struct3 &a, const __struct3 &b) {
  return a.orderId == b.orderId && a.customer == b.customer &&
         a.total == b.total;
}
inline bool operator!=(const __struct3 &a, const __struct3 &b) {
  return !(a == b);
}
int main() {
  auto customers = std::vector<__struct1>{__struct1{1, std::string("Alice")},
                                          __struct1{2, std::string("Bob")}};
  auto orders =
      std::vector<__struct2>{__struct2{100, 1, 250}, __struct2{101, 3, 80}};
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
  std::cout << std::string("--- Left Join ---") << std::endl;
  for (auto entry : result) {
    {
      std::cout << std::string("Order");
      std::cout << ' ';
      std::cout << std::boolalpha << entry.orderId;
      std::cout << ' ';
      std::cout << std::string("customer");
      std::cout << ' ';
      std::cout << std::boolalpha << entry.customer;
      std::cout << ' ';
      std::cout << std::string("total");
      std::cout << ' ';
      std::cout << std::boolalpha << entry.total;
      std::cout << std::endl;
    }
  }
  return 0;
}
