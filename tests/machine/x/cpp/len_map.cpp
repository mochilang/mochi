#include <algorithm>
#include <iostream>
#include <map>
#include <numeric>
#include <unordered_map>
#include <utility>
#include <vector>

int main() {
  {
    std::cout << std::boolalpha
              << std::unordered_map<std::string, int>{{std::string("a"), 1},
                                                      {std::string("b"), 2}}
                     .size();
    std::cout << std::endl;
  }
  return 0;
}
