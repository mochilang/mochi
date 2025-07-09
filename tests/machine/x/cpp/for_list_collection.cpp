#include <algorithm>
#include <iostream>
#include <map>
#include <numeric>
#include <unordered_map>
#include <utility>
#include <vector>

int main() {
  for (auto n : std::vector<decltype(1)>{1, 2, 3}) {
    {
      std::cout << std::boolalpha << n;
      std::cout << std::endl;
    }
  }
  return 0;
}
