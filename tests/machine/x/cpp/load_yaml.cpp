#include <fstream>
#include <iostream>
#include <string>
#include <vector>

struct Person {
  std::string name;
  int age;
  std::string email;
};
inline bool operator==(const Person &a, const Person &b) {
  return a.name == b.name && a.age == b.age && a.email == b.email;
}
inline bool operator!=(const Person &a, const Person &b) { return !(a == b); }
inline std::vector<Person> __load_yaml_person(const std::string &path) {
  std::ifstream f(path);
  std::string line;
  std::vector<Person> people;
  Person cur;
  while (std::getline(f, line)) {
    if (line.find("- name:") != std::string::npos) {
      if (!cur.name.empty())
        people.push_back(cur);
      cur = Person();
      cur.name = line.substr(line.find(':') + 2);
    } else if (line.find("age:") != std::string::npos) {
      cur.age = std::stoi(line.substr(line.find(':') + 2));
    } else if (line.find("email:") != std::string::npos) {
      cur.email = line.substr(line.find(':') + 2);
    }
  }
  if (!cur.name.empty())
    people.push_back(cur);
  return people;
}
struct __struct1 {
  decltype(std::declval<Person>().name) name;
  decltype(std::declval<Person>().email) email;
};
inline bool operator==(const __struct1 &a, const __struct1 &b) {
  return a.name == b.name && a.email == b.email;
}
inline bool operator!=(const __struct1 &a, const __struct1 &b) {
  return !(a == b);
}
int main() {
  auto people = __load_yaml_person("../interpreter/valid/people.yaml");
  auto adults = ([&]() {
    std::vector<__struct1> __items;
    for (auto p : people) {
      if (!((p.age >= 18)))
        continue;
      __items.push_back(__struct1{p.name, p.email});
    }
    return __items;
  })();
  for (auto a : adults) {
    {
      std::cout << std::boolalpha << a.name;
      std::cout << ' ';
      std::cout << std::boolalpha << a.email;
      std::cout << std::endl;
    }
  }
  return 0;
}
