#include <iostream>
#include <string>
#include <vector>

struct People {
  decltype(std::string("Alice")) name;
  decltype(30) age;
};
inline bool operator==(const People &a, const People &b) {
  return a.name == b.name && a.age == b.age;
}
inline bool operator!=(const People &a, const People &b) { return !(a == b); }
struct Adult {
  decltype(std::declval<People>().name) name;
  decltype(std::declval<People>().age) age;
  bool is_senior;
};
inline bool operator==(const Adult &a, const Adult &b) {
  return a.name == b.name && a.age == b.age && a.is_senior == b.is_senior;
}
inline bool operator!=(const Adult &a, const Adult &b) { return !(a == b); }
int main() {
  std::vector<People> people = std::vector<People>{
      People{std::string("Alice"), 30}, People{std::string("Bob"), 15},
      People{std::string("Charlie"), 65}, People{std::string("Diana"), 45}};
  auto adults = ([&]() {
    std::vector<Adult> __items;
    for (auto person : people) {
      if (!((person.age >= 18)))
        continue;
      __items.push_back(Adult{person.name, person.age, (person.age >= 60)});
    }
    return __items;
  })();
  std::cout << std::string("--- Adults ---") << std::endl;
  for (auto person : adults) {
    {
      std::cout << std::boolalpha << person.name;
      std::cout << ' ';
      std::cout << std::string("is");
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
