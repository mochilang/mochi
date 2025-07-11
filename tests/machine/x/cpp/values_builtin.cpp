#include <iostream>
#include <unordered_map>
#include <vector>

auto m = std::unordered_map<std::string, decltype(1)>{
    {std::string("a"), 1}, {std::string("b"), 2}, {std::string("c"), 3}};

int main() {
  {
    auto __tmp1 = ([&]() {
      std::vector<int> v;
      for (auto &p : m)
        v.push_back(p.second);
      return v;
    })();
    bool first = true;
    for (const auto &_x : __tmp1) {
      if (!first)
        std::cout << ' ';
      first = false;
      std::cout << std::boolalpha << _x;
    }
    std::cout << std::endl;
  }
  return 0;
}
