#include <iostream>
#include <string>
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
int main() {
  auto people = std::vector<decltype(Person{std::string("Alice"), 17,
                                            std::string("minor")})>{
      Person{std::string("Alice"), 17, std::string("minor")},
      Person{std::string("Bob"), 25, std::string("unknown")},
      Person{std::string("Charlie"), 18, std::string("unknown")},
      Person{std::string("Diana"), 16, std::string("minor")}};
  for (auto &__tmp1 : people) {
    if ((__tmp1.age >= 18)) {
      __tmp1.status = std::string("adult");
      __tmp1.age = (__tmp1.age + 1);
    }
  }
  // test update adult status
  std::cout << std::string("ok") << std::endl;
  return 0;
}
