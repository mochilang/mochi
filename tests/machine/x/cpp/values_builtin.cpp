#include <iostream>
#include <unordered_map>
#include <vector>

int main() {
  auto m = std::unordered_map<std::string, decltype(1)>{
      {std::string("a"), 1}, {std::string("b"), 2}, {std::string("c"), 3}};
  {
    auto __tmp1 = ([&]() {
      std::vector<int> v;
      for (auto &p : m)
        v.push_back(p.second);
      return v;
    })();
    for (size_t i = 0; i < __tmp1.size(); ++i) {
      if (i)
        std::cout << ' ';
      std::cout << std::boolalpha << __tmp1[i];
    }
    std::cout << std::endl;
  }
  return 0;
}
