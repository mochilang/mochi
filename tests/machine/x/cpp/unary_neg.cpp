#include <algorithm>
#include <iostream>
#include <map>
#include <numeric>
#include <unordered_map>
#include <utility>
#include <vector>

int main() {
  {
    std::cout << std::boolalpha << (-3);
    std::cout << std::endl;
  }
  {
    std::cout << std::boolalpha << (5 + ((-2)));
    std::cout << std::endl;
  }
  return 0;
}
