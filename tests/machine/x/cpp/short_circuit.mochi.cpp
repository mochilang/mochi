#include <iostream>
#include <vector>
#include <unordered_map>
#include <map>
#include <algorithm>
#include <numeric>

template<typename T> void print(const T& v){ std::cout << v; }
void print(const std::vector<int>& v){ for(size_t i=0;i<v.size();++i){ if(i) std::cout<<' '; std::cout<<v[i]; }}
void print(bool b){ std::cout<<(b?"true":"false"); }

auto boom(auto a, auto b) {
    print("boom");
    return true;
}

int main() {
    print((false && boom(1, 2)));
    print((true || boom(1, 2)));
    return 0;
}
