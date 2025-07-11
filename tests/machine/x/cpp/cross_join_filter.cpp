#include <iostream>
#include <string>
#include <vector>

struct __struct1 {
  decltype(n) n;
  std::string l;
};
inline bool operator==(const __struct1 &a, const __struct1 &b) {
  return a.n == b.n && a.l == b.l;
}
inline bool operator!=(const __struct1 &a, const __struct1 &b) {
  return !(a == b);
}
int main() {
  auto nums = std::vector<int>{1, 2, 3};
  std::vector<std::string> letters =
      std::vector<std::string>{std::string("A"), std::string("B")};
  auto pairs = ([&]() {
    std::vector<__struct1> __items;
    for (auto n : nums) {
      for (auto l : letters) {
        if (!(((n % 2) == 0)))
          continue;
        __items.push_back(__struct1{n, l});
      }
    }
    return __items;
  })();
  std::cout << std::string("--- Even pairs ---") << std::endl;
  for (auto p : pairs) {
    {
      std::cout << std::boolalpha << p.n;
      std::cout << ' ';
      std::cout << std::boolalpha << p.l;
      std::cout << std::endl;
    }
  }
  return 0;
}
