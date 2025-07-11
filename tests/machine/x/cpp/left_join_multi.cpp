#include <iostream>
#include <map>
#include <string>
#include <unordered_map>
#include <vector>

template <typename T> void __json(const T &);
inline void __json(int v) { std::cout << v; }
inline void __json(double v) { std::cout << v; }
inline void __json(bool v) { std::cout << (v ? "true" : "false"); }
inline void __json(const std::string &v) { std::cout << "\"" << v << "\""; }
inline void __json(const char *v) { std::cout << "\"" << v << "\""; }
template <typename T> void __json(const std::vector<T> &v) {
  std::cout << "[";
  bool first = true;
  for (const auto &x : v) {
    if (!first)
      std::cout << ",";
    first = false;
    __json(x);
  }
  std::cout << "]";
}
template <typename K, typename V> void __json(const std::map<K, V> &m) {
  std::cout << "{";
  bool first = true;
  for (const auto &kv : m) {
    if (!first)
      std::cout << ",";
    first = false;
    __json(kv.first);
    std::cout << ":";
    __json(kv.second);
  }
  std::cout << "}";
}
template <typename K, typename V>
void __json(const std::unordered_map<K, V> &m) {
  std::cout << "{";
  bool first = true;
  for (const auto &kv : m) {
    if (!first)
      std::cout << ",";
    first = false;
    __json(kv.first);
    std::cout << ":";
    __json(kv.second);
  }
  std::cout << "}";
}

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
  decltype(std::declval<Order>().id) orderId;
  decltype(std::declval<Customer>().name) name;
  Item item;
};
inline bool operator==(const Result &a, const Result &b) {
  return a.orderId == b.orderId && a.name == b.name && a.item == b.item;
}
inline bool operator!=(const Result &a, const Result &b) { return !(a == b); }
inline void __json(const Order &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"id\":";
  __json(v.id);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"customerId\":";
  __json(v.customerId);
  std::cout << "}";
}
inline void __json(const Customer &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"id\":";
  __json(v.id);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"name\":";
  __json(v.name);
  std::cout << "}";
}
inline void __json(const Result &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"orderId\":";
  __json(v.orderId);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"name\":";
  __json(v.name);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"item\":";
  __json(v.item);
  std::cout << "}";
}
inline void __json(const Item &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"orderId\":";
  __json(v.orderId);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"sku\":";
  __json(v.sku);
  std::cout << "}";
}
int main() {
  std::vector<Customer> customers = std::vector<Customer>{
      Customer{1, std::string("Alice")}, Customer{2, std::string("Bob")}};
  std::vector<Order> orders = std::vector<Order>{Order{100, 1}, Order{101, 2}};
  std::vector<Item> items = std::vector<Item>{Item{100, std::string("a")}};
  auto result = ([&]() {
    std::vector<Result> __items;
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
            __items.push_back(Result{o.id, c.name, i});
          }
          if (!__matched1) {
            auto i = std::decay_t<decltype(*(items).begin())>{};
            __items.push_back(Result{o.id, c.name, i});
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
      __json(r.item);
      std::cout << std::endl;
    }
  }
  return 0;
}
