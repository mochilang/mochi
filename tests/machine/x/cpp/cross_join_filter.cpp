#include <algorithm>
#include <iostream>
#include <map>
#include <numeric>
#include <unordered_map>
#include <utility>
#include <vector>

struct __struct1 {
  int n;
  std::string l;
};
int main() {
  std::vector<int> nums = std::vector<decltype(1)>{1, 2, 3};
  std::vector<std::string> letters = std::vector<decltype(std::string("A"))>{
      std::string("A"), std::string("B")};
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
  {
    std::cout << std::boolalpha << std::string("--- Even pairs ---");
    std::cout << std::endl;
  }
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
