#include <algorithm>
#include <iostream>
#include <map>
#include <numeric>
#include <unordered_map>
#include <utility>
#include <vector>

int main() {
  {
    std::cout << std::boolalpha << (6 * 7);
    std::cout << std::endl;
  }
  {
    std::cout << std::boolalpha << (7 / 2);
    std::cout << std::endl;
  }
  {
    std::cout << std::boolalpha << (7 % 2);
    std::cout << std::endl;
  }
  return 0;
}
