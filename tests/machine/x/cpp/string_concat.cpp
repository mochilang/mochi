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
              << (std::string("hello ") + std::string("world"));
    std::cout << std::endl;
  }
  return 0;
}
