// Generated by Mochi compiler v0.10.26 on 2006-01-02T15:04:05Z
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

struct CatalogSale {
  int cs_sold_date_sk;
  int cs_item_sk;
  int cs_bill_cdemo_sk;
  int cs_promo_sk;
  int cs_quantity;
  float cs_list_price;
  float cs_coupon_amt;
  float cs_sales_price;
};
struct CustomerDemo {
  int cd_demo_sk;
  std::string cd_gender;
  std::string cd_marital_status;
  std::string cd_education_status;
};
struct DateDim {
  int d_date_sk;
  int d_year;
};
struct Item {
  int i_item_sk;
  std::string i_item_id;
};
struct Promotion {
  int p_promo_sk;
  std::string p_channel_email;
  std::string p_channel_event;
};
struct CatalogSale {
  decltype(1) cs_sold_date_sk;
  decltype(1) cs_item_sk;
  decltype(1) cs_bill_cdemo_sk;
  decltype(1) cs_promo_sk;
  decltype(10) cs_quantity;
  decltype(100) cs_list_price;
  decltype(5) cs_coupon_amt;
  decltype(95) cs_sales_price;
};
struct CustomerDemographic {
  decltype(1) cd_demo_sk;
  decltype(std::string("M")) cd_gender;
  decltype(std::string("S")) cd_marital_status;
  decltype(std::string("College")) cd_education_status;
};
struct DateDim {
  decltype(1) d_date_sk;
  decltype(2000) d_year;
};
struct Item {
  decltype(1) i_item_sk;
  decltype(std::string("ITEM1")) i_item_id;
};
struct Promotion {
  decltype(1) p_promo_sk;
  decltype(std::string("N")) p_channel_email;
  decltype(std::string("Y")) p_channel_event;
};
struct Result {
  decltype(cs) cs;
  decltype(cd) cd;
  decltype(d) d;
  decltype(i) i;
  decltype(p) p;
};
struct __struct7 {
  decltype(i.i_item_id) key;
  std::vector<Result> items;
};
template <typename T> double __avg(const std::vector<T> &v) {
  if (v.empty())
    return 0;
  double s = 0;
  for (const auto &x : v)
    s += x;
  return s / v.size();
}
struct __struct8 {
  decltype(std::declval<__struct7>().key) i_item_id;
  double agg1;
  double agg2;
  double agg3;
  double agg4;
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
  std::cout << "\"cs_bill_cdemo_sk\":";
  __json(v.cs_bill_cdemo_sk);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"cs_promo_sk\":";
  __json(v.cs_promo_sk);
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
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"cs_coupon_amt\":";
  __json(v.cs_coupon_amt);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"cs_sales_price\":";
  __json(v.cs_sales_price);
  std::cout << "}";
}
inline void __json(const CustomerDemo &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"cd_demo_sk\":";
  __json(v.cd_demo_sk);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"cd_gender\":";
  __json(v.cd_gender);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"cd_marital_status\":";
  __json(v.cd_marital_status);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"cd_education_status\":";
  __json(v.cd_education_status);
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
  std::cout << "\"i_item_id\":";
  __json(v.i_item_id);
  std::cout << "}";
}
inline void __json(const Promotion &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"p_promo_sk\":";
  __json(v.p_promo_sk);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"p_channel_email\":";
  __json(v.p_channel_email);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"p_channel_event\":";
  __json(v.p_channel_event);
  std::cout << "}";
}
inline void __json(const CustomerDemographic &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"cd_demo_sk\":";
  __json(v.cd_demo_sk);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"cd_gender\":";
  __json(v.cd_gender);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"cd_marital_status\":";
  __json(v.cd_marital_status);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"cd_education_status\":";
  __json(v.cd_education_status);
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
  std::cout << "\"cs_bill_cdemo_sk\":";
  __json(v.cs_bill_cdemo_sk);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"cs_promo_sk\":";
  __json(v.cs_promo_sk);
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
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"cs_coupon_amt\":";
  __json(v.cs_coupon_amt);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"cs_sales_price\":";
  __json(v.cs_sales_price);
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
inline void __json(const __struct8 &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"i_item_id\":";
  __json(v.i_item_id);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"agg1\":";
  __json(v.agg1);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"agg2\":";
  __json(v.agg2);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"agg3\":";
  __json(v.agg3);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"agg4\":";
  __json(v.agg4);
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
  std::cout << "\"i_item_id\":";
  __json(v.i_item_id);
  std::cout << "}";
}
inline void __json(const Promotion &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"p_promo_sk\":";
  __json(v.p_promo_sk);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"p_channel_email\":";
  __json(v.p_channel_email);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"p_channel_event\":";
  __json(v.p_channel_event);
  std::cout << "}";
}
inline void __json(const Result &v) {
  bool first = true;
  std::cout << "{";
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"cs\":";
  __json(v.cs);
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"cd\":";
  __json(v.cd);
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
  if (!first)
    std::cout << ",";
  first = false;
  std::cout << "\"p\":";
  __json(v.p);
  std::cout << "}";
}
int main() {
  std::vector<CatalogSale> catalog_sales = {
      CatalogSale{1, 1, 1, 1, 10, 100, 5, 95},
      CatalogSale{1, 2, 2, 2, 5, 50, 2, 48}};
  std::vector<CustomerDemographic> customer_demographics = {
      CustomerDemographic{1, std::string("M"), std::string("S"),
                          std::string("College")},
      CustomerDemographic{2, std::string("F"), std::string("M"),
                          std::string("High School")}};
  std::vector<DateDim> date_dim = {DateDim{1, 2000}};
  std::vector<Item> item = {Item{1, std::string("ITEM1")},
                            Item{2, std::string("ITEM2")}};
  std::vector<Promotion> promotion = {
      Promotion{1, std::string("N"), std::string("Y")},
      Promotion{2, std::string("Y"), std::string("N")}};
  std::vector<Result> result = ([&]() {
    std::vector<__struct7> __groups;
    for (auto cs : catalog_sales) {
      for (auto cd : customer_demographics) {
        if (!((cs.cs_bill_cdemo_sk == cd.cd_demo_sk)))
          continue;
        for (auto d : date_dim) {
          if (!((cs.cs_sold_date_sk == d.d_date_sk)))
            continue;
          for (auto i : item) {
            if (!((cs.cs_item_sk == i.i_item_sk)))
              continue;
            for (auto p : promotion) {
              if (!((cs.cs_promo_sk == p.p_promo_sk)))
                continue;
              if (!((((((cd.cd_gender == std::string("M")) &&
                        (cd.cd_marital_status == std::string("S"))) &&
                       (cd.cd_education_status == std::string("College"))) &&
                      (((p.p_channel_email == std::string("N")) ||
                        (p.p_channel_event == std::string("N"))))) &&
                     (d.d_year == 2000))))
                continue;
              auto __key = i.i_item_id;
              bool __found = false;
              for (auto &__g : __groups) {
                if (__g.key == __key) {
                  __g.items.push_back(Result{cs, cd, d, i, p});
                  __found = true;
                  break;
                }
              }
              if (!__found) {
                __groups.push_back(__struct7{
                    __key, std::vector<Result>{Result{cs, cd, d, i, p}}});
              }
            }
          }
        }
      }
    }
    std::vector<__struct8> __items;
    for (auto &g : __groups) {
      __items.push_back(__struct8{
          g.key, __avg(([&]() {
            std::vector<decltype(std::declval<Result>().cs_quantity)> __items;
            for (auto x : g.items) {
              __items.push_back(x.cs_quantity);
            }
            return __items;
          })()),
          __avg(([&]() {
            std::vector<decltype(std::declval<Result>().cs_list_price)> __items;
            for (auto x : g.items) {
              __items.push_back(x.cs_list_price);
            }
            return __items;
          })()),
          __avg(([&]() {
            std::vector<decltype(std::declval<Result>().cs_coupon_amt)> __items;
            for (auto x : g.items) {
              __items.push_back(x.cs_coupon_amt);
            }
            return __items;
          })()),
          __avg(([&]() {
            std::vector<decltype(std::declval<Result>().cs_sales_price)>
                __items;
            for (auto x : g.items) {
              __items.push_back(x.cs_sales_price);
            }
            return __items;
          })())});
    }
    return __items;
  })();
  (__json(result));
  // test TPCDS Q26 demographic averages
  return 0;
}
