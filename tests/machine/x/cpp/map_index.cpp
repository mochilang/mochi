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
  {
    std::cout << std::boolalpha << m[std::string("b")];
    std::cout << std::endl;
  }
  return 0;
}
