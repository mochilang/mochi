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
              << std::accumulate(std::vector<decltype(1)>{1, 2, 3}.begin(),
                                 std::vector<decltype(1)>{1, 2, 3}.end(), 0);
    std::cout << std::endl;
  }
  return 0;
}
