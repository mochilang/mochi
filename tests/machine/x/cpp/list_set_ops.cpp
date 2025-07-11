#include <algorithm>
#include <iostream>
#include <vector>

int main() {
  {
    auto __tmp1 = ([&](auto a, auto b) {
      a.insert(a.end(), b.begin(), b.end());
      std::sort(a.begin(), a.end());
      a.erase(std::unique(a.begin(), a.end()), a.end());
      return a;
    })(std::vector<int>{1, 2}, std::vector<int>{2, 3});
    bool first = true;
    for (const auto &_x : __tmp1) {
      if (!first)
        std::cout << ' ';
      first = false;
      std::cout << std::boolalpha << _x;
    }
    std::cout << std::endl;
  }
  {
    auto __tmp2 = ([&](auto a, auto b) {
      for (auto &x : b)
        a.erase(std::remove(a.begin(), a.end(), x), a.end());
      return a;
    })(std::vector<int>{1, 2, 3}, std::vector<int>{2});
    bool first = true;
    for (const auto &_x : __tmp2) {
      if (!first)
        std::cout << ' ';
      first = false;
      std::cout << std::boolalpha << _x;
    }
    std::cout << std::endl;
  }
  {
    auto __tmp3 = ([&](auto a, auto b) {
      std::vector<std::decay_t<decltype(a[0])>> r_;
      for (auto &x : a)
        if (std::find(b.begin(), b.end(), x) != b.end())
          r_.push_back(x);
      return r_;
    })(std::vector<int>{1, 2, 3}, std::vector<int>{2, 4});
    bool first = true;
    for (const auto &_x : __tmp3) {
      if (!first)
        std::cout << ' ';
      first = false;
      std::cout << std::boolalpha << _x;
    }
    std::cout << std::endl;
  }
  std::cout << std::boolalpha
            << ([&](auto a, auto b) {
                 a.insert(a.end(), b.begin(), b.end());
                 std::sort(a.begin(), a.end());
                 a.erase(std::unique(a.begin(), a.end()), a.end());
                 return a;
               })(std::vector<int>{1, 2}, std::vector<int>{2, 3})
                   .size()
            << std::endl;
  return 0;
}
