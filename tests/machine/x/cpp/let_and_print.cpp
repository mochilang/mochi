#include <algorithm>
#include <iostream>
#include <map>
#include <numeric>
#include <unordered_map>
#include <utility>
#include <vector>

int main() {
  auto a = 10;
  int b = 20;
  {
    std::cout << std::boolalpha << (a + b);
    std::cout << std::endl;
  }
  return 0;
}
