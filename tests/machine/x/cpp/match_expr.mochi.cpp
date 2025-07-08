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
    auto x = 2;
    auto label = ([&]() { auto __v = x; if (__v == 1) return "one"; else if (__v == 2) return "two"; else if (__v == 3) return "three"; return "unknown"; })();
    print(label);
    return 0;
}
