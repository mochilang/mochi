// Generated by Mochi transpiler v0.10.33 on 2025-07-21 18:20:05 GMT+7
#include <iostream>
#include <string>
#include <vector>
#include <sstream>
#include <iomanip>


auto matrix = std::vector{std::vector{1, 2}, std::vector{3, 4}};

int main() {
    matrix[1][0] = 5;
    std::cout << std::boolalpha << matrix[1][0] << std::endl;
    return 0;
}
