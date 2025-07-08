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
    print((std::vector<int>{1, 2} union std::vector<int>{2, 3}));
    print((std::vector<int>{1, 2, 3} except std::vector<int>{2}));
    print((std::vector<int>{1, 2, 3} intersect std::vector<int>{2, 4}));
    print((std::vector<int>{1, 2} union std::vector<int>{2, 3}).size());
    return 0;
}
