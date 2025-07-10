#include <algorithm>
#include <iostream>
#include <map>
#include <numeric>
#include <unordered_map>
#include <utility>
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

struct __struct1 {
  decltype(1) n_nationkey;
  decltype(std::string("BRAZIL")) n_name;
};
inline void __json(const __struct1 &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"n_nationkey\":";
  __json(v.n_nationkey);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"n_name\":";
  __json(v.n_name);
  std::cout << "}";
}
struct __struct2 {
  decltype(1) c_custkey;
  decltype(std::string("Alice")) c_name;
  decltype(100) c_acctbal;
  decltype(1) c_nationkey;
  decltype(std::string("123 St")) c_address;
  decltype(std::string("123-456")) c_phone;
  decltype(std::string("Loyal")) c_comment;
};
inline void __json(const __struct2 &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"c_custkey\":";
  __json(v.c_custkey);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"c_name\":";
  __json(v.c_name);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"c_acctbal\":";
  __json(v.c_acctbal);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"c_nationkey\":";
  __json(v.c_nationkey);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"c_address\":";
  __json(v.c_address);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"c_phone\":";
  __json(v.c_phone);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"c_comment\":";
  __json(v.c_comment);
  std::cout << "}";
}
struct __struct3 {
  decltype(1000) o_orderkey;
  decltype(1) o_custkey;
  decltype(std::string("1993-10-15")) o_orderdate;
};
inline void __json(const __struct3 &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"o_orderkey\":";
  __json(v.o_orderkey);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"o_custkey\":";
  __json(v.o_custkey);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"o_orderdate\":";
  __json(v.o_orderdate);
  std::cout << "}";
}
struct __struct4 {
  decltype(1000) l_orderkey;
  decltype(std::string("R")) l_returnflag;
  decltype(1000) l_extendedprice;
  decltype(0.1) l_discount;
};
inline void __json(const __struct4 &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"l_orderkey\":";
  __json(v.l_orderkey);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"l_returnflag\":";
  __json(v.l_returnflag);
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
  std::cout << "}";
}
struct __struct5 {
  decltype(std::declval<__struct2>().c_custkey) c_custkey;
  decltype(std::declval<__struct2>().c_name) c_name;
  decltype(std::declval<__struct2>().c_acctbal) c_acctbal;
  decltype(std::declval<__struct2>().c_address) c_address;
  decltype(std::declval<__struct2>().c_phone) c_phone;
  decltype(std::declval<__struct2>().c_comment) c_comment;
  decltype(std::declval<__struct1>().n_name) n_name;
};
inline void __json(const __struct5 &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"c_custkey\":";
  __json(v.c_custkey);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"c_name\":";
  __json(v.c_name);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"c_acctbal\":";
  __json(v.c_acctbal);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"c_address\":";
  __json(v.c_address);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"c_phone\":";
  __json(v.c_phone);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"c_comment\":";
  __json(v.c_comment);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"n_name\":";
  __json(v.n_name);
  std::cout << "}";
}
struct __struct6 {
  __struct2 c;
  __struct3 o;
  __struct4 l;
  __struct1 n;
};
inline void __json(const __struct6 &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"c\":";
  __json(v.c);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"o\":";
  __json(v.o);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"l\":";
  __json(v.l);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"n\":";
  __json(v.n);
  std::cout << "}";
}
struct __struct7 {
  __struct5 key;
  std::vector<__struct6> items;
};
struct __struct8 {
  decltype(g.key.c_custkey) c_custkey;
  decltype(g.key.c_name) c_name;
  bool revenue;
  decltype(g.key.c_acctbal) c_acctbal;
  decltype(g.key.n_name) n_name;
  decltype(g.key.c_address) c_address;
  decltype(g.key.c_phone) c_phone;
  decltype(g.key.c_comment) c_comment;
};
inline void __json(const __struct8 &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"c_custkey\":";
  __json(v.c_custkey);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"c_name\":";
  __json(v.c_name);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"revenue\":";
  __json(v.revenue);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"c_acctbal\":";
  __json(v.c_acctbal);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"n_name\":";
  __json(v.n_name);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"c_address\":";
  __json(v.c_address);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"c_phone\":";
  __json(v.c_phone);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"c_comment\":";
  __json(v.c_comment);
  std::cout << "}";
}
int main() {
  std::vector<__struct1> nation =
      std::vector<decltype(__struct1{1, std::string("BRAZIL")})>{
          __struct1{1, std::string("BRAZIL")}};
  std::vector<__struct2> customer = std::vector<decltype(__struct2{
      1, std::string("Alice"), 100, 1, std::string("123 St"),
      std::string("123-456"), std::string("Loyal")})>{
      __struct2{1, std::string("Alice"), 100, 1, std::string("123 St"),
                std::string("123-456"), std::string("Loyal")}};
  std::vector<__struct3> orders =
      std::vector<decltype(__struct3{1000, 1, std::string("1993-10-15")})>{
          __struct3{1000, 1, std::string("1993-10-15")},
          __struct3{2000, 1, std::string("1994-01-02")}};
  std::vector<__struct4> lineitem =
      std::vector<decltype(__struct4{1000, std::string("R"), 1000, 0.1})>{
          __struct4{1000, std::string("R"), 1000, 0.1},
          __struct4{2000, std::string("N"), 500, 0}};
  auto start_date = std::string("1993-10-01");
  auto end_date = std::string("1994-01-01");
  auto result = ([&]() {
    std::vector<__struct7> __groups;
    for (auto c : customer) {
      for (auto o : orders) {
        if (!((o.o_custkey == c.c_custkey)))
          continue;
        for (auto l : lineitem) {
          if (!((l.l_orderkey == o.o_orderkey)))
            continue;
          for (auto n : nation) {
            if (!((n.n_nationkey == c.c_nationkey)))
              continue;
            if (!((((o.o_orderdate >= start_date) &&
                    (o.o_orderdate < end_date)) &&
                   (l.l_returnflag == std::string("R")))))
              continue;
            auto __key =
                __struct5{c.c_custkey, c.c_name,    c.c_acctbal, c.c_address,
                          c.c_phone,   c.c_comment, n.n_name};
            bool __found = false;
            for (auto &__g : __groups) {
              if (__g.key == __key) {
                __g.items.push_back(__struct6{c, o, l, n});
                __found = true;
                break;
              }
            }
            if (!__found) {
              __groups.push_back(__struct7{
                  __key, std::vector<__struct6>{__struct6{c, o, l, n}}});
            }
          }
        }
      }
    }
    std::vector<std::pair<decltype((-([&](auto v) {
                            return std::accumulate(
                                v.bestd::declval<__struct7>() in(), v.end(), 0);
                          })(([&]() {
                            std::vector<decltype((
                                std::declval<__struct6>()
                                    .std::declval<__struct4>()
                                    .l_extendedprice *
                                ((1 - std::declval<__struct6>()
                                          .std::declval<__struct4>()
                                          .l_discount))))>
                                __items;
                            for (auto x : std::declval<__struct7>().items) {
                              __items.push_back((x.l.l_extendedprice *
                                                 ((1 - x.l.l_discount))));
                            }
                            return __items;
                          })()))),
                          __struct8>>
        __items;
    for (auto &g : __groups) {
      __items.push_back(
          {(-([&](auto v) { return std::accumulate(v.begin(), v.end(), 0); })(
               ([&]() {
                 std::vector<decltype((std::declval<__struct6>()
                                           .std::declval<__struct4>()
                                           .l_extendedprice *
                                       ((1 - std::declval<__struct6>()
                                                 .std::declval<__struct4>()
                                                 .l_discount))))>
                     __items;
                 for (auto x : g.items) {
                   __items.push_back(
                       (x.l.l_extendedprice * ((1 - x.l.l_discount))));
                 }
                 return __items;
               })())),
           __struct8{
               g.key.c_custkey, g.key.c_name, ([&](auto v) {
                 return std::accumulate(v.begin(), v.end(), 0);
               })(([&]() {
                 std::vector<decltype((std::declval<__struct6>()
                                           .std::declval<__struct4>()
                                           .l_extendedprice *
                                       ((1 - std::declval<__struct6>()
                                                 .std::declval<__struct4>()
                                                 .l_discount))))>
                     __items;
                 for (auto x : g.items) {
                   __items.push_back(
                       (x.l.l_extendedprice * ((1 - x.l.l_discount))));
                 }
                 return __items;
               })()),
               g.key.c_acctbal, g.key.n_name, g.key.c_address, g.key.c_phone,
               g.key.c_comment}});
    }
    std::sort(__items.begin(), __items.end(),
              [](auto &a, auto &b) { return a.first < b.first; });
    std::vector<__struct8> __res;
    for (auto &p : __items)
      __res.push_back(p.second);
    return __res;
  })();
  {
    auto __tmp1 = result;
    for (size_t i = 0; i < __tmp1.size(); ++i) {
      if (i)
        std::cout << ' ';
      std::cout << "<struct>";
    }
    std::cout << std::endl;
  }
  return 0;
}
