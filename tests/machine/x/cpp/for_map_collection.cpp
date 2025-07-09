#include <algorithm>
#include <iostream>
#include <map>
#include <numeric>
#include <unordered_map>
#include <utility>
#include <vector>

int main() {
  auto m = std::unordered_map<std::string, decltype(1)>{{std::string("a"), 1},
                                                        {std::string("b"), 2}};
  for (auto k : m) {
    {
      std::cout << std::boolalpha << k.first << ' ' << k.second;
      std::cout << std::endl;
    }
  }
  return 0;
}
