#include <algorithm>
#include <iostream>
#include <map>
#include <numeric>
#include <unordered_map>
#include <utility>
#include <vector>

auto makeAdder(auto n) {
  return [=](int x) { return (x + n); };
}

int main() {
  auto add10 = makeAdder(10);
  {
    std::cout << std::boolalpha << add10(7);
    std::cout << std::endl;
  }
  return 0;
}
