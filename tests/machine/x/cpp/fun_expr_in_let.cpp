#include <algorithm>
#include <iostream>
#include <map>
#include <numeric>
#include <unordered_map>
#include <utility>
#include <vector>

int main() {
  auto square = [=](int x) { return (x * x); };
  {
    std::cout << std::boolalpha << square(6);
    std::cout << std::endl;
  }
  return 0;
}
