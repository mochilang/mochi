#include <iostream>
#include <vector>
#include <unordered_map>
#include <map>
#include <algorithm>
#include <numeric>

template<typename T> void print(const T& v){ std::cout << v; }
void print(const std::vector<int>& v){ for(size_t i=0;i<v.size();++i){ if(i) std::cout<<' '; std::cout<<v[i]; }}
void print(bool b){ std::cout<<(b?"true":"false"); }

int main() {
    auto matrix = std::vector<int>{std::vector<int>{1, 2}, std::vector<int>{3, 4}};
    matrix[1][0] = 5;
    print(matrix[1][0]);
    return 0;
}
