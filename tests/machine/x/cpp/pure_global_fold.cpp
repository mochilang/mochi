#include <algorithm>
#include <iostream>
#include <map>
#include <numeric>
#include <unordered_map>
#include <utility>
#include <vector>

auto inc(auto x) { return (x + k); }

int main() {
  auto k = 2;
  {
    std::cout << std::boolalpha << inc(3);
    std::cout << std::endl;
  }
  return 0;
}
