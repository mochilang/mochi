#include <iostream>
#include <vector>
#include <unordered_map>
#include <map>
#include <algorithm>
#include <numeric>

template<typename T> void print(const T& v){ std::cout << v; }
void print(const std::vector<int>& v){ for(size_t i=0;i<v.size();++i){ if(i) std::cout<<' '; std::cout<<v[i]; }}
void print(bool b){ std::cout<<(b?"true":"false"); }

auto outer(auto x) {
    auto inner = [=](int y) {
    return (x + y);
};
    return inner(5);
}

int main() {
    print(outer(3));
    return 0;
}
