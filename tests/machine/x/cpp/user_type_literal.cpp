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
auto book = Book{std::string("Go"), Person{std::string("Bob"), 42}};

int main() {
  std::cout << std::boolalpha << book.author.name << std::endl;
  return 0;
}
