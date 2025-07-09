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
              << ((int)std::vector<decltype(1)>{1, 2, 3}.size());
    std::cout << std::endl;
  }
  return 0;
}
