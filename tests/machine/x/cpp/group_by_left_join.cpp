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
struct Stat {
  Customer c;
  Order o;
};
inline bool operator==(const Stat &a, const Stat &b) {
  return a.c == b.c && a.o == b.o;
}
inline bool operator!=(const Stat &a, const Stat &b) { return !(a == b); }
struct __struct4 {
  decltype(std::declval<Customer>().name) key;
  std::vector<Stat> items;
};
inline bool operator==(const __struct4 &a, const __struct4 &b) {
  return a.key == b.key && a.items == b.items;
}
inline bool operator!=(const __struct4 &a, const __struct4 &b) {
  return !(a == b);
}
struct __struct5 {
  decltype(std::declval<__struct4>().key) name;
  int count;
};
inline bool operator==(const __struct5 &a, const __struct5 &b) {
  return a.name == b.name && a.count == b.count;
}
inline bool operator!=(const __struct5 &a, const __struct5 &b) {
  return !(a == b);
}
int main() {
  std::vector<Customer> customers = std::vector<Customer>{
      Customer{1, std::string("Alice")}, Customer{2, std::string("Bob")},
      Customer{3, std::string("Charlie")}};
  std::vector<Order> orders =
      std::vector<Order>{Order{100, 1}, Order{101, 1}, Order{102, 2}};
  auto stats = ([&]() {
    std::vector<__struct4> __groups;
    for (auto c : customers) {
      {
        bool __matched0 = false;
        for (auto o : orders) {
          if (!((o.customerId == c.id)))
            continue;
          __matched0 = true;
          auto __key = c.name;
          bool __found = false;
          for (auto &__g : __groups) {
            if (__g.key == __key) {
              __g.items.push_back(Stat{c, o});
              __found = true;
              break;
            }
          }
          if (!__found) {
            __groups.push_back(__struct4{__key, std::vector<Stat>{Stat{c, o}}});
          }
        }
        if (!__matched0) {
          auto o = std::decay_t<decltype(*(orders).begin())>{};
          auto __key = c.name;
          bool __found = false;
          for (auto &__g : __groups) {
            if (__g.key == __key) {
              __g.items.push_back(Stat{c, o});
              __found = true;
              break;
            }
          }
          if (!__found) {
            __groups.push_back(__struct4{__key, std::vector<Stat>{Stat{c, o}}});
          }
        }
      }
    }
    std::vector<__struct5> __items;
    for (auto &g : __groups) {
      __items.push_back(__struct5{g.key, ((int)([&]() {
                                            std::vector<Stat> __items;
                                            for (auto r : g.items) {
                                              if (!((r.o != Order{})))
                                                continue;
                                              __items.push_back(r);
                                            }
                                            return __items;
                                          })()
                                              .size())});
    }
    return __items;
  })();
  std::cout << std::string("--- Group Left Join ---") << std::endl;
  for (auto s : stats) {
    {
      std::cout << std::boolalpha << s.name;
      std::cout << ' ';
      std::cout << std::string("orders:");
      std::cout << ' ';
      std::cout << std::boolalpha << s.count;
      std::cout << std::endl;
    }
  }
  return 0;
}
