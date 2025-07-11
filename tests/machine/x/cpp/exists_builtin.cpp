#include <iostream>
#include <vector>

int main() {
  auto data = std::vector<int>{1, 2};
  auto flag = (!([&]() {
                  std::vector<decltype(x)> __items;
                  for (auto x : data) {
                    if (!((x == 1)))
                      continue;
                    __items.push_back(x);
                  }
                  return __items;
                })()
                    .empty());
  std::cout << std::boolalpha << flag << std::endl;
  return 0;
}
