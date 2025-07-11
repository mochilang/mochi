#include <iostream>
#include <string>
#include <vector>

struct Pair {
  int n;
  std::string l;
};
inline bool operator==(const Pair &a, const Pair &b) {
  return a.n == b.n && a.l == b.l;
}
inline bool operator!=(const Pair &a, const Pair &b) { return !(a == b); }
int main() {
  std::vector<int> nums = std::vector<int>{1, 2, 3};
  std::vector<std::string> letters =
      std::vector<std::string>{std::string("A"), std::string("B")};
  auto pairs = ([&]() {
    std::vector<Pair> __items;
    for (auto n : nums) {
      for (auto l : letters) {
        if (!(((n % 2) == 0)))
          continue;
        __items.push_back(Pair{n, l});
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
