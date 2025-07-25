// Generated by Mochi compiler v0.10.26 on 2025-07-16T09:21:11Z
#include <iostream>
#include <string>

struct Foobar {
  int Exported;
  int unexported;
};

Foobar examineAndModify(Foobar f) {
  std::cout << ((((((((std::string(" v: {") + std::to_string(f.Exported)) +
                      std::string(" ")) +
                     std::to_string(f.unexported)) +
                    std::string("} = {")) +
                   std::to_string(f.Exported)) +
                  std::string(" ")) +
                 std::to_string(f.unexported)) +
                std::string("}"))
            << std::endl;
  std::cout << std::string("    Idx Name       Type CanSet") << std::endl;
  std::cout << std::string("     0: Exported   int  true") << std::endl;
  std::cout << std::string("     1: unexported int  false") << std::endl;
  f.Exported = 16;
  f.unexported = 44;
  std::cout << std::string("  modified unexported field via unsafe")
            << std::endl;
  return f;
}

auto anotherExample() {
  std::cout << std::string("bufio.ReadByte returned error: unsafely injected "
                           "error value into bufio inner workings")
            << std::endl;
}

int main() {
  auto obj = Foobar{12, 42};
  std::cout << ((((std::string("obj: {") + std::to_string(obj.Exported)) +
                  std::string(" ")) +
                 std::to_string(obj.unexported)) +
                std::string("}"))
            << std::endl;
  obj = examineAndModify(obj);
  std::cout << ((((std::string("obj: {") + std::to_string(obj.Exported)) +
                  std::string(" ")) +
                 std::to_string(obj.unexported)) +
                std::string("}"))
            << std::endl;
  anotherExample();
  return 0;
}
