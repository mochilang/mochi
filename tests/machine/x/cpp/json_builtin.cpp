#include <algorithm>
#include <iostream>
#include <map>
#include <numeric>
#include <unordered_map>
#include <utility>
#include <vector>

struct __struct1 {
  decltype(1) a;
  decltype(2) b;
};
int main() {
  auto m = __struct1{1, 2};
  json(m);
  return 0;
}
