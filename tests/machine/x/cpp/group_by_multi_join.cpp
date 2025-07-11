#include <iostream>
#include <numeric>
#include <string>
#include <vector>

struct __struct1 {
  decltype(1) id;
  decltype(std::string("A")) name;
};
inline bool operator==(const __struct1 &a, const __struct1 &b) {
  return a.id == b.id && a.name == b.name;
}
inline bool operator!=(const __struct1 &a, const __struct1 &b) {
  return !(a == b);
}
struct __struct2 {
  decltype(1) id;
  decltype(1) nation;
};
inline bool operator==(const __struct2 &a, const __struct2 &b) {
  return a.id == b.id && a.nation == b.nation;
}
inline bool operator!=(const __struct2 &a, const __struct2 &b) {
  return !(a == b);
}
struct __struct3 {
  decltype(100) part;
  decltype(1) supplier;
  decltype(10) cost;
  decltype(2) qty;
};
inline bool operator==(const __struct3 &a, const __struct3 &b) {
  return a.part == b.part && a.supplier == b.supplier && a.cost == b.cost &&
         a.qty == b.qty;
}
inline bool operator!=(const __struct3 &a, const __struct3 &b) {
  return !(a == b);
}
struct __struct4 {
  decltype(ps.part) part;
  decltype((ps.cost * ps.qty)) value;
};
inline bool operator==(const __struct4 &a, const __struct4 &b) {
  return a.part == b.part && a.value == b.value;
}
inline bool operator!=(const __struct4 &a, const __struct4 &b) {
  return !(a == b);
}
struct __struct5 {
  decltype(std::declval<__struct4>().part) key;
  std::vector<__struct4> items;
};
inline bool operator==(const __struct5 &a, const __struct5 &b) {
  return a.key == b.key && a.items == b.items;
}
inline bool operator!=(const __struct5 &a, const __struct5 &b) {
  return !(a == b);
}
struct __struct6 {
  decltype(std::declval<__struct5>().key) part;
  bool total;
};
inline bool operator==(const __struct6 &a, const __struct6 &b) {
  return a.part == b.part && a.total == b.total;
}
inline bool operator!=(const __struct6 &a, const __struct6 &b) {
  return !(a == b);
}
int main() {
  auto nations = std::vector<__struct1>{__struct1{1, std::string("A")},
                                        __struct1{2, std::string("B")}};
  auto suppliers = std::vector<__struct2>{__struct2{1, 1}, __struct2{2, 2}};
  auto partsupp =
      std::vector<__struct3>{__struct3{100, 1, 10, 2}, __struct3{100, 2, 20, 1},
                             __struct3{200, 1, 5, 3}};
  auto filtered = ([&]() {
    std::vector<__struct4> __items;
    for (auto ps : partsupp) {
      for (auto s : suppliers) {
        if (!((s.id == ps.supplier)))
          continue;
        for (auto n : nations) {
          if (!((n.id == s.nation)))
            continue;
          if (!((n.name == std::string("A"))))
            continue;
          __items.push_back(__struct4{ps.part, (ps.cost * ps.qty)});
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
          __g.items.push_back(__struct4{x});
          __found = true;
          break;
        }
      }
      if (!__found) {
        __groups.push_back(
            __struct5{__key, std::vector<__struct4>{__struct4{x}}});
      }
    }
    std::vector<__struct6> __items;
    for (auto &g : __groups) {
      __items.push_back(__struct6{
          g.key, ([&](auto v) {
            return std::accumulate(v.begin(), v.end(), 0);
          })(([&]() {
            std::vector<decltype(std::declval<__struct4>().value)> __items;
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
