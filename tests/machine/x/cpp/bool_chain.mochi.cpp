#include <iostream>
#include <vector>
#include <unordered_map>
#include <map>
#include <algorithm>
#include <numeric>

template<typename T> void print(const T& v){ std::cout << v; }
void print(const std::vector<int>& v){ for(size_t i=0;i<v.size();++i){ if(i) std::cout<<' '; std::cout<<v[i]; }}
void print(bool b){ std::cout<<(b?"true":"false"); }

auto boom() {
    print("boom");
    return true;
}

int main() {
    print(((((1 < 2)) && ((2 < 3))) && ((3 < 4))));
    print(((((1 < 2)) && ((2 > 3))) && boom()));
    print((((((1 < 2)) && ((2 < 3))) && ((3 > 4))) && boom()));
    return 0;
}
