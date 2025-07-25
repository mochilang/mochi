// Generated by Mochi compiler v0.10.26 on 2006-01-02T15:04:05Z
#include <algorithm>
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

struct StoreSale {
  int ss_item_sk;
  int ss_sold_date_sk;
  int ss_customer_sk;
  int ss_quantity;
  float ss_sales_price;
};
struct DateDim {
  int d_date_sk;
  int d_year;
  int d_moy;
};
struct Item {
  int i_item_sk;
};
struct CatalogSale {
  int cs_sold_date_sk;
  int cs_item_sk;
  int cs_bill_customer_sk;
  int cs_quantity;
  float cs_list_price;
};
struct WebSale {
  int ws_sold_date_sk;
  int ws_item_sk;
  int ws_bill_customer_sk;
  int ws_quantity;
  float ws_list_price;
};
struct StoreSale {
  decltype(1) ss_item_sk;
  decltype(1) ss_sold_date_sk;
  decltype(1) ss_customer_sk;
  decltype(1) ss_quantity;
  decltype(10) ss_sales_price;
};
struct DateDim {
  decltype(1) d_date_sk;
  decltype(2000) d_year;
  decltype(1) d_moy;
};
struct CatalogSale {
  decltype(1) cs_sold_date_sk;
  decltype(1) cs_item_sk;
  decltype(1) cs_bill_customer_sk;
  decltype(2) cs_quantity;
  decltype(10) cs_list_price;
};
struct WebSale {
  decltype(1) ws_sold_date_sk;
  decltype(1) ws_item_sk;
  decltype(1) ws_bill_customer_sk;
  decltype(3) ws_quantity;
  decltype(10) ws_list_price;
};
struct FrequentSsItem {
  decltype(i.i_item_sk) item_sk;
  decltype(d.d_date_sk) date_sk;
};
struct __struct6 {
  decltype(ss) ss;
  decltype(d) d;
  std::string i;
};
struct __struct7 {
  FrequentSsItem key;
  std::vector<__struct6> items;
};
struct CustomerTotal {
  decltype(ss) ss;
};
struct __struct9 {
  decltype(std::declval<CustomerTotal>().ss_customer_sk) key;
  std::vector<CustomerTotal> items;
};
struct __struct10 {
  decltype(std::declval<__struct9>().key) cust;
  double sales;
};
inline void __json(const CatalogSale &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"cs_sold_date_sk\":";
  __json(v.cs_sold_date_sk);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"cs_item_sk\":";
  __json(v.cs_item_sk);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"cs_bill_customer_sk\":";
  __json(v.cs_bill_customer_sk);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"cs_quantity\":";
  __json(v.cs_quantity);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"cs_list_price\":";
  __json(v.cs_list_price);
  std::cout << "}";
}
inline void __json(const DateDim &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"d_date_sk\":";
  __json(v.d_date_sk);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"d_year\":";
  __json(v.d_year);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"d_moy\":";
  __json(v.d_moy);
  std::cout << "}";
}
inline void __json(const Item &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"i_item_sk\":";
  __json(v.i_item_sk);
  std::cout << "}";
}
inline void __json(const StoreSale &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"ss_item_sk\":";
  __json(v.ss_item_sk);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"ss_sold_date_sk\":";
  __json(v.ss_sold_date_sk);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"ss_customer_sk\":";
  __json(v.ss_customer_sk);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"ss_quantity\":";
  __json(v.ss_quantity);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"ss_sales_price\":";
  __json(v.ss_sales_price);
  std::cout << "}";
}
inline void __json(const WebSale &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"ws_sold_date_sk\":";
  __json(v.ws_sold_date_sk);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"ws_item_sk\":";
  __json(v.ws_item_sk);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"ws_bill_customer_sk\":";
  __json(v.ws_bill_customer_sk);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"ws_quantity\":";
  __json(v.ws_quantity);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"ws_list_price\":";
  __json(v.ws_list_price);
  std::cout << "}";
}
inline void __json(const CatalogSale &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"cs_sold_date_sk\":";
  __json(v.cs_sold_date_sk);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"cs_item_sk\":";
  __json(v.cs_item_sk);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"cs_bill_customer_sk\":";
  __json(v.cs_bill_customer_sk);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"cs_quantity\":";
  __json(v.cs_quantity);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"cs_list_price\":";
  __json(v.cs_list_price);
  std::cout << "}";
}
inline void __json(const __struct10 &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"cust\":";
  __json(v.cust);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"sales\":";
  __json(v.sales);
  std::cout << "}";
}
inline void __json(const DateDim &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"d_date_sk\":";
  __json(v.d_date_sk);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"d_year\":";
  __json(v.d_year);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"d_moy\":";
  __json(v.d_moy);
  std::cout << "}";
}
inline void __json(const FrequentSsItem &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"item_sk\":";
  __json(v.item_sk);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"date_sk\":";
  __json(v.date_sk);
  std::cout << "}";
}
inline void __json(const StoreSale &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"ss_item_sk\":";
  __json(v.ss_item_sk);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"ss_sold_date_sk\":";
  __json(v.ss_sold_date_sk);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"ss_customer_sk\":";
  __json(v.ss_customer_sk);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"ss_quantity\":";
  __json(v.ss_quantity);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"ss_sales_price\":";
  __json(v.ss_sales_price);
  std::cout << "}";
}
inline void __json(const CustomerTotal &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"ss\":";
  __json(v.ss);
  std::cout << "}";
}
inline void __json(const __struct6 &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"ss\":";
  __json(v.ss);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"d\":";
  __json(v.d);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"i\":";
  __json(v.i);
  std::cout << "}";
}
inline void __json(const WebSale &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"ws_sold_date_sk\":";
  __json(v.ws_sold_date_sk);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"ws_item_sk\":";
  __json(v.ws_item_sk);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"ws_bill_customer_sk\":";
  __json(v.ws_bill_customer_sk);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"ws_quantity\":";
  __json(v.ws_quantity);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"ws_list_price\":";
  __json(v.ws_list_price);
  std::cout << "}";
}
int main() {
  std::vector<StoreSale> store_sales = {
      StoreSale{1, 1, 1, 1, 10}, StoreSale{1, 1, 1, 1, 10},
      StoreSale{1, 1, 1, 1, 10}, StoreSale{1, 1, 1, 1, 10},
      StoreSale{1, 1, 1, 1, 10}, StoreSale{2, 1, 2, 1, 10},
      StoreSale{2, 1, 2, 1, 10}, StoreSale{2, 1, 2, 1, 10}};
  std::vector<DateDim> date_dim = {DateDim{1, 2000, 1}};
  std::vector<std::string> item =
      std::vector<decltype(std::unordered_map<std::string, decltype(1)>{
          {std::string("i_item_sk"), 1}})>{
          std::unordered_map<std::string, decltype(1)>{
              {std::string("i_item_sk"), 1}},
          std::unordered_map<std::string, decltype(2)>{
              {std::string("i_item_sk"), 2}}};
  std::vector<CatalogSale> catalog_sales = {CatalogSale{1, 1, 1, 2, 10},
                                            CatalogSale{1, 2, 2, 2, 10}};
  std::vector<WebSale> web_sales = {WebSale{1, 1, 1, 3, 10},
                                    WebSale{1, 2, 2, 1, 10}};
  std::vector<__struct6> frequent_ss_items = ([&]() {
    std::vector<__struct7> __groups;
    for (auto ss : store_sales) {
      for (auto d : date_dim) {
        if (!((ss.ss_sold_date_sk == d.d_date_sk)))
          continue;
        for (auto i : item) {
          if (!((ss.ss_item_sk == i.i_item_sk)))
            continue;
          if (!((d.d_year == 2000)))
            continue;
          auto __key = FrequentSsItem{i.i_item_sk, d.d_date_sk};
          bool __found = false;
          for (auto &__g : __groups) {
            if (__g.key == __key) {
              __g.items.push_back(__struct6{ss, d, i});
              __found = true;
              break;
            }
          }
          if (!__found) {
            __groups.push_back(
                __struct7{__key, std::vector<__struct6>{__struct6{ss, d, i}}});
          }
        }
      }
    }
    std::vector<decltype(std::declval<__struct7>().key.item_sk)> __items;
    for (auto &g : __groups) {
      if (!((((int)g.items.size()) > 4)))
        continue;
      __items.push_back(g.key.item_sk);
    }
    return __items;
  })();
  std::vector<CustomerTotal> customer_totals = ([&]() {
    std::vector<__struct9> __groups;
    for (auto ss : store_sales) {
      auto __key = ss.ss_customer_sk;
      bool __found = false;
      for (auto &__g : __groups) {
        if (__g.key == __key) {
          __g.items.push_back(CustomerTotal{ss});
          __found = true;
          break;
        }
      }
      if (!__found) {
        __groups.push_back(
            __struct9{__key, std::vector<CustomerTotal>{CustomerTotal{ss}}});
      }
    }
    std::vector<__struct10> __items;
    for (auto &g : __groups) {
      __items.push_back(
          __struct10{g.key, ([&](auto v) {
                       return std::accumulate(v.begin(), v.end(), 0.0);
                     })(([&]() {
                       std::vector<decltype((
                           std::declval<CustomerTotal>().ss_quantity *
                           std::declval<CustomerTotal>().ss_sales_price))>
                           __items;
                       for (auto x : g.items) {
                         __items.push_back((x.ss_quantity * x.ss_sales_price));
                       }
                       return __items;
                     })())});
    }
    return __items;
  })();
  auto max_sales = (*std::max_element(
      ([&]() {
        std::vector<decltype(std::declval<CustomerTotal>().sales)> __items;
        for (auto c : customer_totals) {
          __items.push_back(c.sales);
        }
        return __items;
      })()
          .begin(),
      ([&]() {
        std::vector<decltype(std::declval<CustomerTotal>().sales)> __items;
        for (auto c : customer_totals) {
          __items.push_back(c.sales);
        }
        return __items;
      })()
          .end()));
  auto best_ss_customer = ([&]() {
    std::vector<decltype(std::declval<CustomerTotal>().cust)> __items;
    for (auto c : customer_totals) {
      if (!((c.sales > (0.95 * max_sales))))
        continue;
      __items.push_back(c.cust);
    }
    return __items;
  })();
  auto catalog = ([&]() {
    std::vector<decltype((cs.cs_quantity * cs.cs_list_price))> __items;
    for (auto cs : catalog_sales) {
      for (auto d : date_dim) {
        if (!((cs.cs_sold_date_sk == d.d_date_sk)))
          continue;
        if (!(((((d.d_year == 2000) && (d.d_moy == 1)) &&
                (std::find(best_ss_customer.begin(), best_ss_customer.end(),
                           cs.cs_bill_customer_sk) !=
                 best_ss_customer.end())) &&
               (std::find(frequent_ss_items.begin(), frequent_ss_items.end(),
                          cs.cs_item_sk) != frequent_ss_items.end()))))
          continue;
        __items.push_back((cs.cs_quantity * cs.cs_list_price));
      }
    }
    return __items;
  })();
  auto web = ([&]() {
    std::vector<decltype((ws.ws_quantity * ws.ws_list_price))> __items;
    for (auto ws : web_sales) {
      for (auto d : date_dim) {
        if (!((ws.ws_sold_date_sk == d.d_date_sk)))
          continue;
        if (!(((((d.d_year == 2000) && (d.d_moy == 1)) &&
                (std::find(best_ss_customer.begin(), best_ss_customer.end(),
                           ws.ws_bill_customer_sk) !=
                 best_ss_customer.end())) &&
               (std::find(frequent_ss_items.begin(), frequent_ss_items.end(),
                          ws.ws_item_sk) != frequent_ss_items.end()))))
          continue;
        __items.push_back((ws.ws_quantity * ws.ws_list_price));
      }
    }
    return __items;
  })();
  auto result =
      (([&](auto v) { return std::accumulate(v.begin(), v.end(), 0.0); })(
           catalog) +
       ([&](auto v) { return std::accumulate(v.begin(), v.end(), 0.0); })(web));
  (__json(result));
  // test TPCDS Q23 cross-channel sales
  return 0;
}
