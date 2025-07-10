#include <iostream>
#include <vector>

std::vector<int> data = std::vector<decltype(1)>{1, 2};
auto flag = (!([]() {
                std::vector<int> __items;
                for (auto x : data) {
                  if (!((x == 1)))
                    continue;
                  __items.push_back(x);
                }
                return __items;
              })()
                  .empty());

int main() {
  std::cout << std::boolalpha << flag << std::endl;
  return 0;
}
