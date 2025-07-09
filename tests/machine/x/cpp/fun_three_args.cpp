#include <algorithm>
#include <iostream>
#include <map>
#include <numeric>
#include <unordered_map>
#include <utility>
#include <vector>

auto sum3(auto a, auto b, auto c) { return ((a + b) + c); }

int main() {
  {
    std::cout << std::boolalpha << sum3(1, 2, 3);
    std::cout << std::endl;
  }
  return 0;
}
