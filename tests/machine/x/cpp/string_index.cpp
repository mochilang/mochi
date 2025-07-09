#include <algorithm>
#include <iostream>
#include <map>
#include <numeric>
#include <unordered_map>
#include <utility>
#include <vector>

int main() {
  auto s = std::string("mochi");
  {
    std::cout << std::boolalpha << s[1];
    std::cout << std::endl;
  }
  return 0;
}
