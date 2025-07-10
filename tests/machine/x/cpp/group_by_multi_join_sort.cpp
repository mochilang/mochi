#include <algorithm>
#include <iostream>
#include <numeric>
#include <utility>
#include <vector>

struct __struct1 {
  decltype(1) n_nationkey;
  decltype(std::string("BRAZIL")) n_name;
};
inline bool operator==(const __struct1 &a, const __struct1 &b) {
  return a.n_nationkey == b.n_nationkey && a.n_name == b.n_name;
}
inline bool operator!=(const __struct1 &a, const __struct1 &b) {
  return !(a == b);
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
inline bool operator==(const __struct2 &a, const __struct2 &b) {
  return a.c_custkey == b.c_custkey && a.c_name == b.c_name &&
         a.c_acctbal == b.c_acctbal && a.c_nationkey == b.c_nationkey &&
         a.c_address == b.c_address && a.c_phone == b.c_phone &&
         a.c_comment == b.c_comment;
}
inline bool operator!=(const __struct2 &a, const __struct2 &b) {
  return !(a == b);
}
struct __struct3 {
  decltype(1000) o_orderkey;
  decltype(1) o_custkey;
  decltype(std::string("1993-10-15")) o_orderdate;
};
inline bool operator==(const __struct3 &a, const __struct3 &b) {
  return a.o_orderkey == b.o_orderkey && a.o_custkey == b.o_custkey &&
         a.o_orderdate == b.o_orderdate;
}
inline bool operator!=(const __struct3 &a, const __struct3 &b) {
  return !(a == b);
}
struct __struct4 {
  decltype(1000) l_orderkey;
  decltype(std::string("R")) l_returnflag;
  decltype(1000) l_extendedprice;
  decltype(0.1) l_discount;
};
inline bool operator==(const __struct4 &a, const __struct4 &b) {
  return a.l_orderkey == b.l_orderkey && a.l_returnflag == b.l_returnflag &&
         a.l_extendedprice == b.l_extendedprice && a.l_discount == b.l_discount;
}
inline bool operator!=(const __struct4 &a, const __struct4 &b) {
  return !(a == b);
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
inline bool operator==(const __struct5 &a, const __struct5 &b) {
  return a.c_custkey == b.c_custkey && a.c_name == b.c_name &&
         a.c_acctbal == b.c_acctbal && a.c_address == b.c_address &&
         a.c_phone == b.c_phone && a.c_comment == b.c_comment &&
         a.n_name == b.n_name;
}
inline bool operator!=(const __struct5 &a, const __struct5 &b) {
  return !(a == b);
}
struct __struct6 {
  __struct2 c;
  __struct3 o;
  __struct4 l;
  __struct1 n;
};
inline bool operator==(const __struct6 &a, const __struct6 &b) {
  return a.c == b.c && a.o == b.o && a.l == b.l && a.n == b.n;
}
inline bool operator!=(const __struct6 &a, const __struct6 &b) {
  return !(a == b);
}
struct __struct7 {
  __struct5 key;
  std::vector<__struct6> items;
};
inline bool operator==(const __struct7 &a, const __struct7 &b) {
  return a.key == b.key && a.items == b.items;
}
inline bool operator!=(const __struct7 &a, const __struct7 &b) {
  return !(a == b);
}
struct __struct8 {
  decltype(std::declval<__struct7>().key.c_custkey) c_custkey;
  decltype(std::declval<__struct7>().key.c_name) c_name;
  bool revenue;
  decltype(std::declval<__struct7>().key.c_acctbal) c_acctbal;
  decltype(std::declval<__struct7>().key.n_name) n_name;
  decltype(std::declval<__struct7>().key.c_address) c_address;
  decltype(std::declval<__struct7>().key.c_phone) c_phone;
  decltype(std::declval<__struct7>().key.c_comment) c_comment;
};
inline bool operator==(const __struct8 &a, const __struct8 &b) {
  return a.c_custkey == b.c_custkey && a.c_name == b.c_name &&
         a.revenue == b.revenue && a.c_acctbal == b.c_acctbal &&
         a.n_name == b.n_name && a.c_address == b.c_address &&
         a.c_phone == b.c_phone && a.c_comment == b.c_comment;
}
inline bool operator!=(const __struct8 &a, const __struct8 &b) {
  return !(a == b);
}
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
auto result = ([]() {
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
  std::vector<std::pair<double, __struct8>> __items;
  for (auto &g : __groups) {
    __items.push_back(
        {(-([&](auto v) {
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
             __items.push_back((x.l.l_extendedprice * ((1 - x.l.l_discount))));
           }
           return __items;
         })())),
         __struct8{g.key.c_custkey, g.key.c_name, ([&](auto v) {
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
                   g.key.c_acctbal, g.key.n_name, g.key.c_address,
                   g.key.c_phone, g.key.c_comment}});
  }
  std::sort(__items.begin(), __items.end(),
            [](auto &a, auto &b) { return a.first < b.first; });
  std::vector<__struct8> __res;
  for (auto &p : __items)
    __res.push_back(p.second);
  return __res;
})();

int main() {
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
