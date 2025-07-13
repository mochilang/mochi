#include <iostream>
#include <map>
#include <numeric>
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

struct Lineitem {
  decltype(17) l_quantity;
  decltype(1000) l_extendedprice;
  decltype(0.05) l_discount;
  decltype(0.07) l_tax;
  decltype(std::string("N")) l_returnflag;
  decltype(std::string("O")) l_linestatus;
  decltype(std::string("1998-08-01")) l_shipdate;
};
inline bool operator==(const Lineitem &a, const Lineitem &b) {
  return a.l_quantity == b.l_quantity &&
         a.l_extendedprice == b.l_extendedprice &&
         a.l_discount == b.l_discount && a.l_tax == b.l_tax &&
         a.l_returnflag == b.l_returnflag && a.l_linestatus == b.l_linestatus &&
         a.l_shipdate == b.l_shipdate;
}
inline bool operator!=(const Lineitem &a, const Lineitem &b) {
  return !(a == b);
}
struct Result {
  decltype(std::declval<Lineitem>().l_returnflag) returnflag;
  decltype(std::declval<Lineitem>().l_linestatus) linestatus;
};
inline bool operator==(const Result &a, const Result &b) {
  return a.returnflag == b.returnflag && a.linestatus == b.linestatus;
}
inline bool operator!=(const Result &a, const Result &b) { return !(a == b); }
struct __struct3 {
  Result key;
  std::vector<Lineitem> items;
};
inline bool operator==(const __struct3 &a, const __struct3 &b) {
  return a.key == b.key && a.items == b.items;
}
inline bool operator!=(const __struct3 &a, const __struct3 &b) {
  return !(a == b);
}
template <typename T> double __avg(const std::vector<T> &v) {
  if (v.empty())
    return 0;
  double s = 0;
  for (const auto &x : v)
    s += x;
  return s / v.size();
}
struct __struct4 {
  decltype(std::declval<__struct3>().key.returnflag) returnflag;
  decltype(std::declval<__struct3>().key.linestatus) linestatus;
  double sum_qty;
  double sum_base_price;
  double sum_disc_price;
  double sum_charge;
  double avg_qty;
  double avg_price;
  double avg_disc;
  int count_order;
};
inline bool operator==(const __struct4 &a, const __struct4 &b) {
  return a.returnflag == b.returnflag && a.linestatus == b.linestatus &&
         a.sum_qty == b.sum_qty && a.sum_base_price == b.sum_base_price &&
         a.sum_disc_price == b.sum_disc_price && a.sum_charge == b.sum_charge &&
         a.avg_qty == b.avg_qty && a.avg_price == b.avg_price &&
         a.avg_disc == b.avg_disc && a.count_order == b.count_order;
}
inline bool operator!=(const __struct4 &a, const __struct4 &b) {
  return !(a == b);
}
inline void __json(const Lineitem &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"l_quantity\":";
  __json(v.l_quantity);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"l_extendedprice\":";
  __json(v.l_extendedprice);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"l_discount\":";
  __json(v.l_discount);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"l_tax\":";
  __json(v.l_tax);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"l_returnflag\":";
  __json(v.l_returnflag);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"l_linestatus\":";
  __json(v.l_linestatus);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"l_shipdate\":";
  __json(v.l_shipdate);
  std::cout << "}";
}
inline void __json(const Result &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"returnflag\":";
  __json(v.returnflag);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"linestatus\":";
  __json(v.linestatus);
  std::cout << "}";
}
inline void __json(const __struct4 &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"returnflag\":";
  __json(v.returnflag);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"linestatus\":";
  __json(v.linestatus);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"sum_qty\":";
  __json(v.sum_qty);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"sum_base_price\":";
  __json(v.sum_base_price);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"sum_disc_price\":";
  __json(v.sum_disc_price);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"sum_charge\":";
  __json(v.sum_charge);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"avg_qty\":";
  __json(v.avg_qty);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"avg_price\":";
  __json(v.avg_price);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"avg_disc\":";
  __json(v.avg_disc);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"count_order\":";
  __json(v.count_order);
  std::cout << "}";
}
int main() {
  std::vector<Lineitem> lineitem = std::vector<Lineitem>{
      Lineitem{17, 1000, 0.05, 0.07, std::string("N"), std::string("O"),
               std::string("1998-08-01")},
      Lineitem{36, 2000, 0.1, 0.05, std::string("N"), std::string("O"),
               std::string("1998-09-01")},
      Lineitem{25, 1500, 0, 0.08, std::string("R"), std::string("F"),
               std::string("1998-09-03")}};
  auto result = ([&]() {
    std::vector<__struct3> __groups;
    for (auto row : lineitem) {
      if (!((row.l_shipdate <= std::string("1998-09-02"))))
        continue;
      auto __key = Result{row.l_returnflag, row.l_linestatus};
      bool __found = false;
      for (auto &__g : __groups) {
        if (__g.key == __key) {
          __g.items.push_back(Lineitem{row});
          __found = true;
          break;
        }
      }
      if (!__found) {
        __groups.push_back(
            __struct3{__key, std::vector<Lineitem>{Lineitem{row}}});
      }
    }
    std::vector<__struct4> __items;
    for (auto &g : __groups) {
      __items.push_back(__struct4{
          g.key.returnflag, g.key.linestatus, ([&](auto v) {
            return std::accumulate(v.begin(), v.end(), 0.0);
          })(([&]() {
            std::vector<decltype(std::declval<Lineitem>().l_quantity)> __items;
            for (auto x : g.items) {
              __items.push_back(x.l_quantity);
            }
            return __items;
          })()),
          ([&](auto v) { return std::accumulate(v.begin(), v.end(), 0.0); })(
              ([&]() {
                std::vector<decltype(std::declval<Lineitem>().l_extendedprice)>
                    __items;
                for (auto x : g.items) {
                  __items.push_back(x.l_extendedprice);
                }
                return __items;
              })()),
          ([&](auto v) {
            return std::accumulate(v.begin(), v.end(), 0.0);
          })(([&]() {
            std::vector<decltype((std::declval<Lineitem>().l_extendedprice *
                                  ((1 - std::declval<Lineitem>().l_discount))))>
                __items;
            for (auto x : g.items) {
              __items.push_back((x.l_extendedprice * ((1 - x.l_discount))));
            }
            return __items;
          })()),
          ([&](auto v) {
            return std::accumulate(v.begin(), v.end(), 0.0);
          })(([&]() {
            std::vector<decltype((
                (std::declval<Lineitem>().l_extendedprice *
                 ((1 - std::declval<Lineitem>().l_discount))) *
                ((1 + std::declval<Lineitem>().l_tax))))>
                __items;
            for (auto x : g.items) {
              __items.push_back(((x.l_extendedprice * ((1 - x.l_discount))) *
                                 ((1 + x.l_tax))));
            }
            return __items;
          })()),
          __avg(([&]() {
            std::vector<decltype(std::declval<Lineitem>().l_quantity)> __items;
            for (auto x : g.items) {
              __items.push_back(x.l_quantity);
            }
            return __items;
          })()),
          __avg(([&]() {
            std::vector<decltype(std::declval<Lineitem>().l_extendedprice)>
                __items;
            for (auto x : g.items) {
              __items.push_back(x.l_extendedprice);
            }
            return __items;
          })()),
          __avg(([&]() {
            std::vector<decltype(std::declval<Lineitem>().l_discount)> __items;
            for (auto x : g.items) {
              __items.push_back(x.l_discount);
            }
            return __items;
          })()),
          ((int)g.items.size())});
    }
    return __items;
  })();
  (__json(result));
  // test Q1 aggregates revenue and quantity by returnflag + linestatus
  return 0;
}
