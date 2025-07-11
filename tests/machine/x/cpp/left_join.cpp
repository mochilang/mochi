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
  decltype(250) total;
};
inline bool operator==(const Order &a, const Order &b) {
  return a.id == b.id && a.customerId == b.customerId && a.total == b.total;
}
inline bool operator!=(const Order &a, const Order &b) { return !(a == b); }
struct Result {
  decltype(std::declval<Order>().id) orderId;
  Customer customer;
  decltype(std::declval<Order>().total) total;
};
inline bool operator==(const Result &a, const Result &b) {
  return a.orderId == b.orderId && a.customer == b.customer &&
         a.total == b.total;
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
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"total\":";
  __json(v.total);
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
  std::cout << "\"customer\":";
  __json(v.customer);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"total\":";
  __json(v.total);
  std::cout << "}";
}
int main() {
  std::vector<Customer> customers = std::vector<Customer>{
      Customer{1, std::string("Alice")}, Customer{2, std::string("Bob")}};
  std::vector<Order> orders =
      std::vector<Order>{Order{100, 1, 250}, Order{101, 3, 80}};
  auto result = ([&]() {
    std::vector<Result> __items;
    for (auto o : orders) {
      {
        bool __matched0 = false;
        for (auto c : customers) {
          if (!((o.customerId == c.id)))
            continue;
          __matched0 = true;
          __items.push_back(Result{o.id, c, o.total});
        }
        if (!__matched0) {
          auto c = std::decay_t<decltype(*(customers).begin())>{};
          __items.push_back(Result{o.id, c, o.total});
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
      __json(entry.customer);
      std::cout << ' ';
      std::cout << std::string("total");
      std::cout << ' ';
      std::cout << std::boolalpha << entry.total;
      std::cout << std::endl;
    }
  }
  return 0;
}
