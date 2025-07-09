#include <algorithm>
#include <iostream>
#include <map>
#include <numeric>
#include <unordered_map>
#include <utility>
#include <vector>

int main() {
  auto x = 12;
  auto msg = ((x > 10) ? std::string("yes") : std::string("no"));
  {
    std::cout << std::boolalpha << msg;
    std::cout << std::endl;
  }
  return 0;
}
