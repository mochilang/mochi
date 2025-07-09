#include <algorithm>
#include <iostream>
#include <map>
#include <numeric>
#include <unordered_map>
#include <utility>
#include <vector>

int main() {
  {
    std::cout
        << std::boolalpha
        << std::string(std::vector<decltype(1)>{1, 2, 3}).substr(1, (3) - (1));
    std::cout << std::endl;
  }
  {
    std::cout
        << std::boolalpha
        << std::string(std::vector<decltype(1)>{1, 2, 3}).substr(0, (2) - (0));
    std::cout << std::endl;
  }
  {
    std::cout << std::boolalpha
              << std::string(std::string("hello")).substr(1, (4) - (1));
    std::cout << std::endl;
  }
  return 0;
}
