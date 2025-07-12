#include <iostream>
#include <numeric>
#include <string>
#include <vector>

struct Nation {
  decltype(1) id;
  decltype(std::string("A")) name;
};
inline bool operator==(const Nation &a, const Nation &b) {
  return a.id == b.id && a.name == b.name;
}
inline bool operator!=(const Nation &a, const Nation &b) { return !(a == b); }
struct Supplier {
  decltype(1) id;
  decltype(1) nation;
};
inline bool operator==(const Supplier &a, const Supplier &b) {
  return a.id == b.id && a.nation == b.nation;
}
inline bool operator!=(const Supplier &a, const Supplier &b) {
  return !(a == b);
}
struct Partsupp {
  decltype(100) part;
  decltype(1) supplier;
  decltype(10) cost;
  decltype(2) qty;
};
inline bool operator==(const Partsupp &a, const Partsupp &b) {
  return a.part == b.part && a.supplier == b.supplier && a.cost == b.cost &&
         a.qty == b.qty;
}
inline bool operator!=(const Partsupp &a, const Partsupp &b) {
  return !(a == b);
}
struct Filtered {
  decltype(std::declval<Partsupp>().part) part;
  decltype((std::declval<Partsupp>().cost *
            std::declval<Partsupp>().qty)) value;
};
inline bool operator==(const Filtered &a, const Filtered &b) {
  return a.part == b.part && a.value == b.value;
}
inline bool operator!=(const Filtered &a, const Filtered &b) {
  return !(a == b);
}
struct __struct5 {
  decltype(std::declval<Filtered>().part) key;
  std::vector<Filtered> items;
};
inline bool operator==(const __struct5 &a, const __struct5 &b) {
  return a.key == b.key && a.items == b.items;
}
inline bool operator!=(const __struct5 &a, const __struct5 &b) {
  return !(a == b);
}
struct Grouped {
  decltype(std::declval<__struct5>().key) part;
  double total;
};
inline bool operator==(const Grouped &a, const Grouped &b) {
  return a.part == b.part && a.total == b.total;
}
inline bool operator!=(const Grouped &a, const Grouped &b) { return !(a == b); }
int main() {
  std::vector<Nation> nations = std::vector<Nation>{
      Nation{1, std::string("A")}, Nation{2, std::string("B")}};
  std::vector<Supplier> suppliers =
      std::vector<Supplier>{Supplier{1, 1}, Supplier{2, 2}};
  std::vector<Partsupp> partsupp = std::vector<Partsupp>{
      Partsupp{100, 1, 10, 2}, Partsupp{100, 2, 20, 1}, Partsupp{200, 1, 5, 3}};
  auto filtered = ([&]() {
    std::vector<Filtered> __items;
    for (auto ps : partsupp) {
      for (auto s : suppliers) {
        if (!((s.id == ps.supplier)))
          continue;
        for (auto n : nations) {
          if (!((n.id == s.nation)))
            continue;
          if (!((n.name == std::string("A"))))
            continue;
          __items.push_back(Filtered{ps.part, (ps.cost * ps.qty)});
        }
      }
    }
    return __items;
  })();
  auto grouped = ([&]() {
    std::vector<__struct5> __groups;
    for (auto x : filtered) {
      auto __key = x.part;
      bool __found = false;
      for (auto &__g : __groups) {
        if (__g.key == __key) {
          __g.items.push_back(Filtered{x});
          __found = true;
          break;
        }
      }
      if (!__found) {
        __groups.push_back(
            __struct5{__key, std::vector<Filtered>{Filtered{x}}});
      }
    }
    std::vector<Grouped> __items;
    for (auto &g : __groups) {
      __items.push_back(Grouped{
          g.key, ([&](auto v) {
            return std::accumulate(v.begin(), v.end(), 0.0);
          })(([&]() {
            std::vector<decltype(std::declval<Filtered>().value)> __items;
            for (auto r : g.items) {
              __items.push_back(r.value);
            }
            return __items;
          })())});
    }
    return __items;
  })();
  {
    auto __tmp1 = grouped;
    bool first = true;
    for (const auto &_x : __tmp1) {
      if (!first)
        std::cout << ' ';
      first = false;
      std::cout << "<struct>";
    }
    std::cout << std::endl;
  }
  return 0;
}
