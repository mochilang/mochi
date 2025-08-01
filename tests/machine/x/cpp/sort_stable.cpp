// Generated by Mochi compiler v0.10.28 on 2006-01-02T15:04:05Z
#include <algorithm>
#include <iostream>
#include <string>
#include <utility>
#include <vector>

struct Item {
  int n;
  std::string v;
};
int main() {
  std::vector<Item> items = {Item{1, std::string("a")},
                             Item{1, std::string("b")},
                             Item{2, std::string("c")}};
  auto result = ([&]() {
    std::vector<std::pair<decltype(std::declval<Item>().n), std::string>>
        __items;
    for (auto i : items) {
      __items.push_back({i.n, i.v});
    }
    std::sort(__items.begin(), __items.end(),
              [](auto &a, auto &b) { return a.first < b.first; });
    std::vector<std::string> __res;
    for (auto &p : __items)
      __res.push_back(p.second);
    return __res;
  })();
  std::cout << "[";
  for (size_t i = 0; i < result.size(); ++i) {
    if (i)
      std::cout << ", ";
    std::cout << result[i];
  }
  std::cout << "]" << std::endl;
  return 0;
}
