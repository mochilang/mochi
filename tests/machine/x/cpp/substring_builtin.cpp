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
              << std::string(std::string("mochi")).substr(1, (4) - (1));
    std::cout << std::endl;
  }
  return 0;
}
