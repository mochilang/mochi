// Generated by Mochi compiler v0.10.28 on 2006-01-02T15:04:05Z
#include <any>
#include <iostream>
#include <string>

inline bool __any_eq(const std::any &a, const std::any &b) {
  if (a.type() != b.type())
    return false;
  if (a.type() == typeid(int))
    return std::any_cast<int>(a) == std::any_cast<int>(b);
  if (a.type() == typeid(double))
    return std::any_cast<double>(a) == std::any_cast<double>(b);
  if (a.type() == typeid(bool))
    return std::any_cast<bool>(a) == std::any_cast<bool>(b);
  if (a.type() == typeid(std::string))
    return std::any_cast<std::string>(a) == std::any_cast<std::string>(b);
  return false;
}
inline void __print_any(const std::any &a) {
  if (a.type() == typeid(int))
    std::cout << std::any_cast<int>(a);
  else if (a.type() == typeid(double))
    std::cout << std::any_cast<double>(a);
  else if (a.type() == typeid(bool))
    std::cout << (std::any_cast<bool>(a) ? "true" : "false");
  else if (a.type() == typeid(std::string))
    std::cout << std::any_cast<std::string>(a);
}

struct Person {
  std::string name;
  int age;
};
struct Book {
  std::string title;
  Person author;
};
int main() {
  auto book = Book{std::string("Go"), Person{std::string("Bob"), 42}};
  std::cout << book.author.name << std::endl;
  return 0;
}
