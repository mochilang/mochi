#include <iostream>
#include <vector>

struct Person {
  std::string name;
  int age;
  std::string status;
};
inline bool operator==(const Person &a, const Person &b) {
  return a.name == b.name && a.age == b.age && a.status == b.status;
}
inline bool operator!=(const Person &a, const Person &b) { return !(a == b); }
struct __struct1 {
  decltype(std::string("Alice")) name;
  decltype(17) age;
  decltype(std::string("minor")) status;
};
inline bool operator==(const __struct1 &a, const __struct1 &b) {
  return a.name == b.name && a.age == b.age && a.status == b.status;
}
inline bool operator!=(const __struct1 &a, const __struct1 &b) {
  return !(a == b);
}
auto people = std::vector<decltype(__struct1{std::string("Alice"), 17,
                                             std::string("minor")})>{
    __struct1{std::string("Alice"), 17, std::string("minor")},
    __struct1{std::string("Bob"), 25, std::string("unknown")},
    __struct1{std::string("Charlie"), 18, std::string("unknown")},
    __struct1{std::string("Diana"), 16, std::string("minor")}};

int main() {
  for (auto &__tmp1 : people) {
    if ((__tmp1.age >= 18)) {
      __tmp1.status = std::string("adult");
      __tmp1.age = (__tmp1.age + 1);
    }
  }
  // test update adult status
  {
    std::cout << std::boolalpha << std::string("ok");
    std::cout << std::endl;
  }
  return 0;
}
