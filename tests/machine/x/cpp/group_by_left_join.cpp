#include <iostream>
#include <vector>

struct __struct1 {
  decltype(1) id;
  decltype(std::string("Alice")) name;
};
inline bool operator==(const __struct1 &a, const __struct1 &b) {
  return a.id == b.id && a.name == b.name;
}
inline bool operator!=(const __struct1 &a, const __struct1 &b) {
  return !(a == b);
}
struct __struct2 {
  decltype(100) id;
  decltype(1) customerId;
};
inline bool operator==(const __struct2 &a, const __struct2 &b) {
  return a.id == b.id && a.customerId == b.customerId;
}
inline bool operator!=(const __struct2 &a, const __struct2 &b) {
  return !(a == b);
}
struct __struct3 {
  __struct1 c;
  __struct2 o;
};
inline bool operator==(const __struct3 &a, const __struct3 &b) {
  return a.c == b.c && a.o == b.o;
}
inline bool operator!=(const __struct3 &a, const __struct3 &b) {
  return !(a == b);
}
struct __struct4 {
  decltype(std::declval<__struct1>().name) key;
  std::vector<__struct3> items;
};
inline bool operator==(const __struct4 &a, const __struct4 &b) {
  return a.key == b.key && a.items == b.items;
}
inline bool operator!=(const __struct4 &a, const __struct4 &b) {
  return !(a == b);
}
struct __struct5 {
  decltype(std::declval<__struct4>().key) name;
  bool count;
};
inline bool operator==(const __struct5 &a, const __struct5 &b) {
  return a.name == b.name && a.count == b.count;
}
inline bool operator!=(const __struct5 &a, const __struct5 &b) {
  return !(a == b);
}
std::vector<__struct1> customers =
    std::vector<decltype(__struct1{1, std::string("Alice")})>{
        __struct1{1, std::string("Alice")}, __struct1{2, std::string("Bob")},
        __struct1{3, std::string("Charlie")}};
std::vector<__struct2> orders = std::vector<decltype(__struct2{100, 1})>{
    __struct2{100, 1}, __struct2{101, 1}, __struct2{102, 2}};
auto stats = ([]() {
  std::vector<__struct4> __groups;
  for (auto c : customers) {
    {
      bool __matched0 = false;
      for (auto o : orders) {
        if (!((o.customerId == c.id)))
          continue;
        __matched0 = true;
        auto __key = c.name;
        bool __found = false;
        for (auto &__g : __groups) {
          if (__g.key == __key) {
            __g.items.push_back(__struct3{c, o});
            __found = true;
            break;
          }
        }
        if (!__found) {
          __groups.push_back(
              __struct4{__key, std::vector<__struct3>{__struct3{c, o}}});
        }
      }
      if (!__matched0) {
        auto o = std::decay_t<decltype(*(orders).begin())>{};
        auto __key = c.name;
        bool __found = false;
        for (auto &__g : __groups) {
          if (__g.key == __key) {
            __g.items.push_back(__struct3{c, o});
            __found = true;
            break;
          }
        }
        if (!__found) {
          __groups.push_back(
              __struct4{__key, std::vector<__struct3>{__struct3{c, o}}});
        }
      }
    }
  }
  std::vector<__struct5> __items;
  for (auto &g : __groups) {
    __items.push_back(__struct5{g.key, ((int)([&]() {
                                          std::vector<__struct3> __items;
                                          for (auto r : g.items) {
                                            if (!((r.o != __struct2{})))
                                              continue;
                                            __items.push_back(r);
                                          }
                                          return __items;
                                        })()
                                            .size())});
  }
  return __items;
})();

int main() {
  std::cout << std::string("--- Group Left Join ---") << std::endl;
  for (auto s : stats) {
    {
      std::cout << std::boolalpha << s.name;
      std::cout << ' ';
      std::cout << std::string("orders:");
      std::cout << ' ';
      std::cout << std::boolalpha << s.count;
      std::cout << std::endl;
    }
  }
  return 0;
}
