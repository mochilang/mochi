#include <iostream>
#include <vector>

struct __struct1 {
  decltype(std::string("Alice")) name;
  decltype(30) age;
};
struct __struct2 {
  decltype(std::declval<__struct1>().name) name;
  decltype(std::declval<__struct1>().age) age;
  bool is_senior;
};
int main() {
  std::vector<__struct1> people =
      std::vector<decltype(__struct1{std::string("Alice"), 30})>{
          __struct1{std::string("Alice"), 30},
          __struct1{std::string("Bob"), 15},
          __struct1{std::string("Charlie"), 65},
          __struct1{std::string("Diana"), 45}};
  auto adults = ([&]() {
    std::vector<__struct2> __items;
    for (auto person : people) {
      if (!((person.age >= 18)))
        continue;
      __items.push_back(__struct2{person.name, person.age, (person.age >= 60)});
    }
    return __items;
  })();
  {
    std::cout << std::boolalpha << std::string("--- Adults ---");
    std::cout << std::endl;
  }
  for (auto person : adults) {
    {
      std::cout << std::boolalpha << person.name;
      std::cout << ' ';
      std::cout << std::boolalpha << std::string("is");
      std::cout << ' ';
      std::cout << std::boolalpha << person.age;
      std::cout << ' ';
      std::cout << std::boolalpha
                << (person.is_senior ? std::string(" (senior)")
                                     : std::string(""));
      std::cout << std::endl;
    }
  }
  return 0;
}
