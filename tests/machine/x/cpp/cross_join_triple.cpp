#include <iostream>
#include <vector>

struct __struct1 {
  int n;
  std::string l;
  bool b;
};
int main() {
  std::vector<int> nums = std::vector<decltype(1)>{1, 2};
  std::vector<std::string> letters = std::vector<decltype(std::string("A"))>{
      std::string("A"), std::string("B")};
  std::vector<bool> bools = std::vector<decltype(true)>{true, false};
  auto combos = ([&]() {
    std::vector<__struct1> __items;
    for (auto n : nums) {
      for (auto l : letters) {
        for (auto b : bools) {
          __items.push_back(__struct1{n, l, b});
        }
      }
    }
    return __items;
  })();
  {
    std::cout << std::boolalpha
              << std::string("--- Cross Join of three lists ---");
    std::cout << std::endl;
  }
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
