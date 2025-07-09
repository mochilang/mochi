#include <algorithm>
#include <iostream>
#include <map>
#include <numeric>
#include <unordered_map>
#include <utility>
#include <vector>

struct __struct1 {
  decltype(Leaf) left;
  decltype(2) value;
  decltype(Leaf) right;
};
auto sum_tree(auto t) {
  return ([&]() {
    auto __v = t;
    if (__v == Leaf)
      return 0;
    else if (__v == Node(left, value, right))
      return ((sum_tree(left) + value) + sum_tree(right));
    return decltype(0){};
  })();
}

int main() {
  auto t = __struct1{Leaf, 1, __struct1{Leaf, 2, Leaf}};
  {
    std::cout << std::boolalpha << sum_tree(t);
    std::cout << std::endl;
  }
  return 0;
}
