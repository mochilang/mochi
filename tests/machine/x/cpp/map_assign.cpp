#include <algorithm>
#include <iostream>
#include <map>
#include <numeric>
#include <unordered_map>
#include <utility>
#include <vector>

int main() {
  auto scores = std::unordered_map<std::string, int>{{std::string("alice"), 1}};
  scores[std::string("bob")] = 2;
  {
    std::cout << std::boolalpha << scores[std::string("bob")];
    std::cout << std::endl;
  }
  return 0;
}
