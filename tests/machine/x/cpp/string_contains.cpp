#include <algorithm>
#include <iostream>
#include <map>
#include <numeric>
#include <unordered_map>
#include <utility>
#include <vector>

int main() {
  auto s = std::string("catch");
  {
    std::cout << std::boolalpha << s.contains(std::string("cat"));
    std::cout << std::endl;
  }
  {
    std::cout << std::boolalpha << s.contains(std::string("dog"));
    std::cout << std::endl;
  }
  return 0;
}
