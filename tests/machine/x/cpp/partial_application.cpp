#include <algorithm>
#include <iostream>
#include <map>
#include <numeric>
#include <unordered_map>
#include <utility>
#include <vector>

auto add(auto a, auto b) { return (a + b); }

int main() {
  auto add5 = add(5);
  {
    std::cout << std::boolalpha << add5(3);
    std::cout << std::endl;
  }
  return 0;
}
