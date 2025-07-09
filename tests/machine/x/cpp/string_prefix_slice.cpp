#include <algorithm>
#include <iostream>
#include <map>
#include <numeric>
#include <unordered_map>
#include <utility>
#include <vector>

int main() {
  auto prefix = std::string("fore");
  auto s1 = std::string("forest");
  {
    std::cout << std::boolalpha << (s1[0] == prefix);
    std::cout << std::endl;
  }
  auto s2 = std::string("desert");
  {
    std::cout << std::boolalpha << (s2[0] == prefix);
    std::cout << std::endl;
  }
  return 0;
}
