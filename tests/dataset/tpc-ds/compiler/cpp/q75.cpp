// Generated by Mochi compiler v0.10.26 on 2006-01-02T15:04:05Z
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

struct DateDim {
  decltype(1) d_date_sk;
  decltype(2000) d_year;
};
struct StoreSale {
  decltype(1) ss_item_sk;
  decltype(50) ss_quantity;
  decltype(500) ss_sales_price;
  decltype(1) ss_sold_date_sk;
};
struct WebSale {
  decltype(1) ws_item_sk;
  decltype(30) ws_quantity;
  decltype(300) ws_sales_price;
  decltype(1) ws_sold_date_sk;
};
struct CatalogSale {
  decltype(1) cs_item_sk;
  decltype(20) cs_quantity;
  decltype(200) cs_sales_price;
  decltype(1) cs_sold_date_sk;
};
struct Item {
  decltype(1) i_item_sk;
  decltype(1) i_brand_id;
  decltype(2) i_class_id;
  decltype(3) i_category_id;
  decltype(4) i_manufact_id;
  decltype(std::string("Electronics")) i_category;
};
struct SalesDetail {
  decltype(d.d_year) d_year;
  decltype(ss.ss_item_sk) i_item_sk;
  decltype(ss.ss_quantity) quantity;
  decltype(ss.ss_sales_price) amount;
};
struct AllSale {
  decltype(std::declval<SalesDetail>().d_year) year;
  decltype(i.i_brand_id) brand_id;
  decltype(i.i_class_id) class_id;
  decltype(i.i_category_id) category_id;
  decltype(i.i_manufact_id) manuf_id;
};
struct __struct8 {
  SalesDetail sd;
  decltype(i) i;
};
struct __struct9 {
  AllSale key;
  std::vector<__struct8> items;
};
struct __struct10 {
  decltype(std::declval<__struct9>().key.year) d_year;
  decltype(std::declval<__struct9>().key.brand_id) i_brand_id;
  decltype(std::declval<__struct9>().key.class_id) i_class_id;
  decltype(std::declval<__struct9>().key.category_id) i_category_id;
  decltype(std::declval<__struct9>().key.manuf_id) i_manufact_id;
  double sales_cnt;
  double sales_amt;
};
struct Result {
  decltype(std::declval<__struct8>().d_year) prev_year;
  decltype(std::declval<__struct8>().d_year) year;
  decltype(std::declval<__struct8>().i_brand_id) i_brand_id;
  decltype(std::declval<__struct8>().i_class_id) i_class_id;
  decltype(std::declval<__struct8>().i_category_id) i_category_id;
  decltype(std::declval<__struct8>().i_manufact_id) i_manufact_id;
  decltype(std::declval<__struct8>().sales_cnt) prev_yr_cnt;
  decltype(std::declval<__struct8>().sales_cnt) curr_yr_cnt;
  decltype((std::declval<__struct8>().sales_cnt -
            std::declval<__struct8>().sales_cnt)) sales_cnt_diff;
  decltype((std::declval<__struct8>().sales_amt -
            std::declval<__struct8>().sales_amt)) sales_amt_diff;
};
inline void __json(const CatalogSale &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"cs_item_sk\":";
  __json(v.cs_item_sk);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"cs_quantity\":";
  __json(v.cs_quantity);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"cs_sales_price\":";
  __json(v.cs_sales_price);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"cs_sold_date_sk\":";
  __json(v.cs_sold_date_sk);
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
  std::cout << "}";
}
inline void __json(const __struct10 &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"d_year\":";
  __json(v.d_year);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"i_brand_id\":";
  __json(v.i_brand_id);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"i_class_id\":";
  __json(v.i_class_id);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"i_category_id\":";
  __json(v.i_category_id);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"i_manufact_id\":";
  __json(v.i_manufact_id);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"sales_cnt\":";
  __json(v.sales_cnt);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"sales_amt\":";
  __json(v.sales_amt);
  std::cout << "}";
}
inline void __json(const SalesDetail &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"d_year\":";
  __json(v.d_year);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"i_item_sk\":";
  __json(v.i_item_sk);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"quantity\":";
  __json(v.quantity);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"amount\":";
  __json(v.amount);
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
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"i_brand_id\":";
  __json(v.i_brand_id);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"i_class_id\":";
  __json(v.i_class_id);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"i_category_id\":";
  __json(v.i_category_id);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"i_manufact_id\":";
  __json(v.i_manufact_id);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"i_category\":";
  __json(v.i_category);
  std::cout << "}";
}
inline void __json(const Result &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"prev_year\":";
  __json(v.prev_year);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"year\":";
  __json(v.year);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"i_brand_id\":";
  __json(v.i_brand_id);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"i_class_id\":";
  __json(v.i_class_id);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"i_category_id\":";
  __json(v.i_category_id);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"i_manufact_id\":";
  __json(v.i_manufact_id);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"prev_yr_cnt\":";
  __json(v.prev_yr_cnt);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"curr_yr_cnt\":";
  __json(v.curr_yr_cnt);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"sales_cnt_diff\":";
  __json(v.sales_cnt_diff);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"sales_amt_diff\":";
  __json(v.sales_amt_diff);
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
  std::cout << "\"ss_quantity\":";
  __json(v.ss_quantity);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"ss_sales_price\":";
  __json(v.ss_sales_price);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"ss_sold_date_sk\":";
  __json(v.ss_sold_date_sk);
  std::cout << "}";
}
inline void __json(const __struct8 &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"sd\":";
  __json(v.sd);
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
  std::cout << "\"ws_item_sk\":";
  __json(v.ws_item_sk);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"ws_quantity\":";
  __json(v.ws_quantity);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"ws_sales_price\":";
  __json(v.ws_sales_price);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"ws_sold_date_sk\":";
  __json(v.ws_sold_date_sk);
  std::cout << "}";
}
inline void __json(const AllSale &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"year\":";
  __json(v.year);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"brand_id\":";
  __json(v.brand_id);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"class_id\":";
  __json(v.class_id);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"category_id\":";
  __json(v.category_id);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"manuf_id\":";
  __json(v.manuf_id);
  std::cout << "}";
}
int main() {
  std::vector<DateDim> date_dim = {DateDim{1, 2000}, DateDim{2, 2001}};
  std::vector<StoreSale> store_sales = {StoreSale{1, 50, 500, 1},
                                        StoreSale{1, 40, 400, 2}};
  std::vector<WebSale> web_sales = {WebSale{1, 30, 300, 1},
                                    WebSale{1, 25, 250, 2}};
  std::vector<CatalogSale> catalog_sales = {CatalogSale{1, 20, 200, 1},
                                            CatalogSale{1, 15, 150, 2}};
  std::vector<Item> item = {Item{1, 1, 2, 3, 4, std::string("Electronics")}};
  std::vector<SalesDetail> sales_detail = concat(
      ([&]() {
        std::vector<SalesDetail> __items;
        for (auto ss : store_sales) {
          for (auto d : date_dim) {
            if (!((d.d_date_sk == ss.ss_sold_date_sk)))
              continue;
            __items.push_back(SalesDetail{d.d_year, ss.ss_item_sk,
                                          ss.ss_quantity, ss.ss_sales_price});
          }
        }
        return __items;
      })(),
      ([&]() {
        std::vector<SalesDetail> __items;
        for (auto ws : web_sales) {
          for (auto d : date_dim) {
            if (!((d.d_date_sk == ws.ws_sold_date_sk)))
              continue;
            __items.push_back(SalesDetail{d.d_year, ws.ws_item_sk,
                                          ws.ws_quantity, ws.ws_sales_price});
          }
        }
        return __items;
      })(),
      ([&]() {
        std::vector<SalesDetail> __items;
        for (auto cs : catalog_sales) {
          for (auto d : date_dim) {
            if (!((d.d_date_sk == cs.cs_sold_date_sk)))
              continue;
            __items.push_back(SalesDetail{d.d_year, cs.cs_item_sk,
                                          cs.cs_quantity, cs.cs_sales_price});
          }
        }
        return __items;
      })());
  std::vector<__struct8> all_sales = ([&]() {
    std::vector<__struct9> __groups;
    for (auto sd : sales_detail) {
      for (auto i : item) {
        if (!((i.i_item_sk == sd.i_item_sk)))
          continue;
        if (!((i.i_category == std::string("Electronics"))))
          continue;
        auto __key = AllSale{sd.d_year, i.i_brand_id, i.i_class_id,
                             i.i_category_id, i.i_manufact_id};
        bool __found = false;
        for (auto &__g : __groups) {
          if (__g.key == __key) {
            __g.items.push_back(__struct8{sd, i});
            __found = true;
            break;
          }
        }
        if (!__found) {
          __groups.push_back(
              __struct9{__key, std::vector<__struct8>{__struct8{sd, i}}});
        }
      }
    }
    std::vector<__struct10> __items;
    for (auto &g : __groups) {
      __items.push_back(__struct10{
          g.key.year, g.key.brand_id, g.key.class_id, g.key.category_id,
          g.key.manuf_id, ([&](auto v) {
            return std::accumulate(v.begin(), v.end(), 0.0);
          })(([&]() {
            std::vector<decltype(std::declval<__struct8>().sd.quantity)>
                __items;
            for (auto x : g.items) {
              __items.push_back(x.sd.quantity);
            }
            return __items;
          })()),
          ([&](auto v) {
            return std::accumulate(v.begin(), v.end(), 0.0);
          })(([&]() {
            std::vector<decltype(std::declval<__struct8>().sd.amount)> __items;
            for (auto x : g.items) {
              __items.push_back(x.sd.amount);
            }
            return __items;
          })())});
    }
    return __items;
  })();
  auto prev_yr = first(([&]() {
    std::vector<__struct8> __items;
    for (auto a : all_sales) {
      if (!((a.d_year == 2000)))
        continue;
      __items.push_back(a);
    }
    return __items;
  })());
  auto curr_yr = first(([&]() {
    std::vector<__struct8> __items;
    for (auto a : all_sales) {
      if (!((a.d_year == 2001)))
        continue;
      __items.push_back(a);
    }
    return __items;
  })());
  auto result =
      ((((curr_yr.sales_cnt / prev_yr.sales_cnt)) < 0.9)
           ? std::vector<Result>{Result{
                 prev_yr.d_year, curr_yr.d_year, curr_yr.i_brand_id,
                 curr_yr.i_class_id, curr_yr.i_category_id,
                 curr_yr.i_manufact_id, prev_yr.sales_cnt, curr_yr.sales_cnt,
                 (curr_yr.sales_cnt - prev_yr.sales_cnt),
                 (curr_yr.sales_amt - prev_yr.sales_amt)}}
           : std::vector<int>{});
  (__json(result));
  // test TPCDS Q75 simplified
  return 0;
}
