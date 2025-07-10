#include <iostream>

auto x = 2;
auto label = ([&]() {
  auto __v = x;
  if (__v == 1)
    return std::string("one");
  else if (__v == 2)
    return std::string("two");
  else if (__v == 3)
    return std::string("three");
  return std::string("unknown");
})();
auto day = std::string("sun");
auto mood = ([&]() {
  auto __v = day;
  if (__v == std::string("mon"))
    return std::string("tired");
  else if (__v == std::string("fri"))
    return std::string("excited");
  else if (__v == std::string("sun"))
    return std::string("relaxed");
  return std::string("normal");
})();
auto ok = true;
auto status = ([&]() {
  auto __v = ok;
  if (__v == true)
    return std::string("confirmed");
  else if (__v == false)
    return std::string("denied");
  return decltype(std::string("confirmed")){};
})();

auto classify(auto n) {
  return ([&]() {
    auto __v = n;
    if (__v == 0)
      return std::string("zero");
    else if (__v == 1)
      return std::string("one");
    return std::string("many");
  })();
}

int main() {
  {
    std::cout << std::boolalpha << label;
    std::cout << std::endl;
  }
  {
    std::cout << std::boolalpha << mood;
    std::cout << std::endl;
  }
  {
    std::cout << std::boolalpha << status;
    std::cout << std::endl;
  }
  {
    std::cout << std::boolalpha << classify(0);
    std::cout << std::endl;
  }
  {
    std::cout << std::boolalpha << classify(5);
    std::cout << std::endl;
  }
  return 0;
}
