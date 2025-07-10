#include <iostream>

struct __struct1 {
  decltype(Leaf) left;
  decltype(2) value;
  decltype(Leaf) right;
};
inline bool operator==(const __struct1 &a, const __struct1 &b) {
  return a.left == b.left && a.value == b.value && a.right == b.right;
}
inline bool operator!=(const __struct1 &a, const __struct1 &b) {
  return !(a == b);
}

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
