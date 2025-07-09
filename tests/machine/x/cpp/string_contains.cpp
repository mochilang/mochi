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
    std::cout << std::boolalpha
              << (s.find(std::string("cat")) != std::string::npos);
    std::cout << std::endl;
  }
  {
    std::cout << std::boolalpha
              << (s.find(std::string("dog")) != std::string::npos);
    std::cout << std::endl;
  }
  return 0;
}
