#include <algorithm>
#include <iostream>
#include <map>
#include <numeric>
#include <unordered_map>
#include <utility>
#include <vector>

template <typename T> void print_val(const T &v) { std::cout << v; }
void print_val(const std::vector<int> &v) {
  for (size_t i = 0; i < v.size(); ++i) {
    if (i)
      std::cout << ' ';
    std::cout << v[i];
  }
}
void print_val(bool b) { std::cout << (b ? "true" : "false"); }
void print() { std::cout << std::endl; }
template <typename First, typename... Rest>
void print(const First &first, const Rest &...rest) {
  print_val(first);
  if constexpr (sizeof...(rest) > 0) {
    std::cout << ' ';
    print(rest...);
  } else {
    std::cout << std::endl;
  }
}

struct __struct1 {
  int n;
  std::string l;
};
int main() {
  auto nums = std::vector<decltype(1)>{1, 2, 3};
  auto letters = std::vector<decltype(std::string("A"))>{std::string("A"),
                                                         std::string("B")};
  auto pairs = ([&]() {
    std::vector<__struct1> __items;
    for (auto n : nums) {
      for (auto l : letters) {
        if (!(((n % 2) == 0)))
          continue;
        __items.push_back(__struct1{n, l});
      }
    }
    return __items;
  })();
  print(std::string("--- Even pairs ---"));
  for (auto p : pairs) {
    print(p.n, p.l);
  }
  return 0;
}
