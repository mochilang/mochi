// Generated by Mochi transpiler v0.10.33 on 2025-07-21 18:20:19 GMT+7
#include <iostream>
#include <string>
#include <sstream>
#include <iomanip>
#include <algorithm>
#include <type_traits>


auto s = std::string("catch");

int main() {
    std::cout << std::boolalpha << ([&](const auto& c, const auto& v){ if constexpr(std::is_same_v<std::decay_t<decltype(c)>, std::string>) { return c.find(v) != std::string::npos; } else if constexpr(requires { c.find(v); }) { return c.find(v) != c.end(); } else { return std::find(c.begin(), c.end(), v) != c.end(); } })(s, std::string("cat")) << std::endl;
    std::cout << std::boolalpha << ([&](const auto& c, const auto& v){ if constexpr(std::is_same_v<std::decay_t<decltype(c)>, std::string>) { return c.find(v) != std::string::npos; } else if constexpr(requires { c.find(v); }) { return c.find(v) != c.end(); } else { return std::find(c.begin(), c.end(), v) != c.end(); } })(s, std::string("dog")) << std::endl;
    return 0;
}
