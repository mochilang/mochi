#include <iostream>
#include <string>
#include <vector>

struct Combo {
  int n;
  std::string l;
  bool b;
};
inline bool operator==(const Combo &a, const Combo &b) {
  return a.n == b.n && a.l == b.l && a.b == b.b;
}
inline bool operator!=(const Combo &a, const Combo &b) { return !(a == b); }
int main() {
  std::vector<int> nums = std::vector<int>{1, 2};
  std::vector<std::string> letters =
      std::vector<std::string>{std::string("A"), std::string("B")};
  std::vector<bool> bools = std::vector<decltype(true)>{true, false};
  auto combos = ([&]() {
    std::vector<Combo> __items;
    for (auto n : nums) {
      for (auto l : letters) {
        for (auto b : bools) {
          __items.push_back(Combo{n, l, b});
        }
      }
    }
    return __items;
  })();
  std::cout << std::string("--- Cross Join of three lists ---") << std::endl;
  for (auto c : combos) {
    {
      std::cout << std::boolalpha << c.n;
      std::cout << ' ';
      std::cout << std::boolalpha << c.l;
      std::cout << ' ';
      std::cout << std::boolalpha << c.b;
      std::cout << std::endl;
    }
  }
  return 0;
}
