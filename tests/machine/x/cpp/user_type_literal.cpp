#include <iostream>

struct Person {
  std::string name;
  int age;
};
inline bool operator==(const Person &a, const Person &b) {
  return a.name == b.name && a.age == b.age;
}
inline bool operator!=(const Person &a, const Person &b) { return !(a == b); }
struct Book {
  std::string title;
  Person author;
};
inline bool operator==(const Book &a, const Book &b) {
  return a.title == b.title && a.author == b.author;
}
inline bool operator!=(const Book &a, const Book &b) { return !(a == b); }
struct __struct1 {
  decltype(std::string("Bob")) name;
  decltype(42) age;
};
inline bool operator==(const __struct1 &a, const __struct1 &b) {
  return a.name == b.name && a.age == b.age;
}
inline bool operator!=(const __struct1 &a, const __struct1 &b) {
  return !(a == b);
}
struct __struct2 {
  decltype(std::string("Go")) title;
  decltype(__struct1{std::string("Bob"), 42}) author;
};
inline bool operator==(const __struct2 &a, const __struct2 &b) {
  return a.title == b.title && a.author == b.author;
}
inline bool operator!=(const __struct2 &a, const __struct2 &b) {
  return !(a == b);
}
auto book = __struct2{std::string("Go"), __struct1{std::string("Bob"), 42}};

int main() {
  {
    std::cout << std::boolalpha << book.author.name;
    std::cout << std::endl;
  }
  return 0;
}
