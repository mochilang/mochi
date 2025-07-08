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
    auto xs = std::vector<int>{1, 2, 3};
    print((std::find(2.begin(), 2.end(), xs) != 2.end()));
    print((!((std::find(5.begin(), 5.end(), xs) != 5.end()))));
    return 0;
}
