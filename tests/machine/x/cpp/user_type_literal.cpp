#include <algorithm>
#include <iostream>
#include <map>
#include <numeric>
#include <unordered_map>
#include <utility>
#include <vector>

struct __struct1 {
  decltype(std::string("Bob")) name;
  decltype(42) age;
};
struct __struct2 {
  decltype(std::string("Go")) title;
  decltype(__struct1{std::string("Bob"), 42}) author;
};
int main() {
  struct Person {
    std::string name;
    int age;
  };
  struct Book {
    std::string title;
    Person author;
  };
  auto book = __struct2{std::string("Go"), __struct1{std::string("Bob"), 42}};
  {
    std::cout << std::boolalpha << book.author.name;
    std::cout << std::endl;
  }
  return 0;
}
