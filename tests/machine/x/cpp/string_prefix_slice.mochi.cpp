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
    auto prefix = "fore";
    auto s1 = "forest";
    print((s1[0] == prefix));
    auto s2 = "desert";
    print((s2[0] == prefix));
    return 0;
}
