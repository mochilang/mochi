#include <algorithm>
#include <iostream>
#include <map>
#include <numeric>
#include <unordered_map>
#include <utility>
#include <vector>

auto triple(auto x) { return (x * 3); }

int main() {
  {
    std::cout << std::boolalpha << triple((1 + 2));
    std::cout << std::endl;
  }
  return 0;
}
