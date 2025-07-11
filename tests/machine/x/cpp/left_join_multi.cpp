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
};
inline bool operator==(const __struct2 &a, const __struct2 &b) {
  return a.id == b.id && a.customerId == b.customerId;
}
inline bool operator!=(const __struct2 &a, const __struct2 &b) {
  return !(a == b);
}
struct __struct3 {
  decltype(100) orderId;
  decltype(std::string("a")) sku;
};
inline bool operator==(const __struct3 &a, const __struct3 &b) {
  return a.orderId == b.orderId && a.sku == b.sku;
}
inline bool operator!=(const __struct3 &a, const __struct3 &b) {
  return !(a == b);
}
struct __struct4 {
  decltype(o.id) orderId;
  decltype(c.name) name;
  decltype(i) item;
};
inline bool operator==(const __struct4 &a, const __struct4 &b) {
  return a.orderId == b.orderId && a.name == b.name && a.item == b.item;
}
inline bool operator!=(const __struct4 &a, const __struct4 &b) {
  return !(a == b);
}
int main() {
  auto customers = std::vector<__struct1>{__struct1{1, std::string("Alice")},
                                          __struct1{2, std::string("Bob")}};
  auto orders = std::vector<__struct2>{__struct2{100, 1}, __struct2{101, 2}};
  auto items = std::vector<__struct3>{__struct3{100, std::string("a")}};
  auto result = ([&]() {
    std::vector<__struct4> __items;
    for (auto o : orders) {
      for (auto c : customers) {
        if (!((o.customerId == c.id)))
          continue;
        {
          bool __matched1 = false;
          for (auto i : items) {
            if (!((o.id == i.orderId)))
              continue;
            __matched1 = true;
            __items.push_back(__struct4{o.id, c.name, i});
          }
          if (!__matched1) {
            auto i = std::decay_t<decltype(*(items).begin())>{};
            __items.push_back(__struct4{o.id, c.name, i});
          }
        }
      }
    }
    return __items;
  })();
  std::cout << std::string("--- Left Join Multi ---") << std::endl;
  for (auto r : result) {
    {
      std::cout << std::boolalpha << r.orderId;
      std::cout << ' ';
      std::cout << std::boolalpha << r.name;
      std::cout << ' ';
      std::cout << std::boolalpha << r.item;
      std::cout << std::endl;
    }
  }
  return 0;
}
