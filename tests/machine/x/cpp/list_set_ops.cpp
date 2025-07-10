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
    })(std::vector<decltype(1)>{1, 2}, std::vector<decltype(2)>{2, 3});
    for (size_t i = 0; i < __tmp1.size(); ++i) {
      if (i)
        std::cout << ' ';
      std::cout << std::boolalpha << __tmp1[i];
    }
    std::cout << std::endl;
  }
  {
    auto __tmp2 = ([&](auto a, auto b) {
      for (auto &x : b)
        a.erase(std::remove(a.begin(), a.end(), x), a.end());
      return a;
    })(std::vector<decltype(1)>{1, 2, 3}, std::vector<decltype(2)>{2});
    for (size_t i = 0; i < __tmp2.size(); ++i) {
      if (i)
        std::cout << ' ';
      std::cout << std::boolalpha << __tmp2[i];
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
    })(std::vector<decltype(1)>{1, 2, 3}, std::vector<decltype(2)>{2, 4});
    for (size_t i = 0; i < __tmp3.size(); ++i) {
      if (i)
        std::cout << ' ';
      std::cout << std::boolalpha << __tmp3[i];
    }
    std::cout << std::endl;
  }
  {
    std::cout << std::boolalpha
              << ([&](auto a, auto b) {
                   a.insert(a.end(), b.begin(), b.end());
                   std::sort(a.begin(), a.end());
                   a.erase(std::unique(a.begin(), a.end()), a.end());
                   return a;
                 })(std::vector<decltype(1)>{1, 2},
                    std::vector<decltype(2)>{2, 3})
                     .size();
    std::cout << std::endl;
  }
  return 0;
}
